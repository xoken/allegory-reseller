{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Reseller.Service.Xoken where

import Allegory.Data
import Codec.Serialise
import Control.Exception
import qualified Control.Exception.Lifted as LE (try)
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control
import Data.Aeson as A
import Data.Aeson.Types (parse)
import qualified Data.ByteString as BS (unpack)
import qualified Data.ByteString.Base16 as B16 (decode, encode)
import Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char (chr)
import Data.List as L
import Data.Maybe
import Data.Serialize
import Data.String (IsString, fromString)
import qualified Data.Text as DT
import qualified Data.Text.Encoding as E
import Data.Word
import Network.HTTP.Req
import Nexa
import Nexa.Constants
import Nexa.Types
import qualified Nexa.Types as AO (AddressOutputs(..))
import qualified NodeConfig as NC
import Prelude
import Reseller.Common
import Reseller.Env
import Reseller.HTTP.Types
import System.Logger as LG
import Xoken

xGetPartiallySignedAllegoryTx ::
       (MonadHttp m, HasResellerEnv env m, MonadIO m)
    => NC.NodeConfig
    -> [(OutPoint', Int)]
    -> ([Int], Bool)
    -> String
    -> String
    -> String
    -> m (BC.ByteString)
xGetPartiallySignedAllegoryTx nodeCnf payips (nameArr, isProducer) owner change buyerUri = do
    lg <- getLogger
    debug lg $ LG.msg $ "xGetPartiallySignedAllegoryTx called: nameArr: " <> (show nameArr)
    bp2pEnv <- getBitcoinP2P
    nodeCfg <- nodeConfig <$> getBitcoinP2P
    sessionKey <- nexaSessionKey <$> getNexaEnv
    nameSecKey <- nameUtxoSecKey <$> getAllegory
    fundSecKey <- fundUtxoSecKey <$> getAllegory
    let net = NC.bitcoinNetwork nodeCfg
    nexaAddr <- (\nc -> return $ NC.nexaListenIP nc <> ":" <> (show $ NC.nexaListenPort nc)) $ (nodeConfig bp2pEnv)
    res <- LE.try $ liftIO $ getProducer nexaAddr sessionKey nameArr isProducer
    producer <-
        case res of
            Left (e :: SomeException) -> do
                err lg $ LG.msg $ "Error: Failed to get producer: " <> (show e)
                throw e
            Right p' -> return p'
    let producerRoot = forName producer
    let nameUtxoSats = NC.nameUtxoSatoshis nodeCfg
        rqFundingSats =
            (getFundingUtxoValue nameUtxoSats) *
            (((length nameArr) - (length producerRoot)) +
             if isProducer
                 then 1
                 else 0 -- will need funding for 1 extra output
             )
    let scr = script producer
    let op = outPoint producer
    --
    --
    let ownerScriptPubKey =
            addressToScriptBS $ fromMaybe (throw KeyValueDBLookupException) $ stringToAddr net $ DT.pack owner
        changeScriptPubKey =
            addressToScriptBS $ fromMaybe (throw KeyValueDBLookupException) $ stringToAddr net $ DT.pack change
        resellerNutxoAddr = pubKeyAddr $ derivePubKeyI $ wrapSecKey True $ nameSecKey
        resellerNutxoScriptPubKey = addressToScriptBS resellerNutxoAddr
        resellerPaymentScriptPubKey = addressToScriptBS resellerNutxoAddr
        resellerFundAddr = pubKeyAddr $ derivePubKeyI $ wrapSecKey True $ fundSecKey
        resellerFundScriptPubKey = addressToScriptBS resellerFundAddr
        resellerFundAddrString = DT.unpack $ fromMaybe "" $ addrToString net resellerFundAddr
        resellerUri = NC.resellerUri nodeCfg
    --
    --
    debug lg $ LG.msg $ "xGetPartiallySignedAllegoryTx got producer root: " <> (show producer)
    debug lg $
        LG.msg $
        "xGetPartiallySignedAllegoryTx needs to make " <> (show $ (length nameArr) - (length producerRoot)) <>
        " interim txns"
    (nameRoot, remFundInput, existed) <-
        if producerRoot == nameArr
            then do
                let rootNameInput =
                        SigInput
                            (addressToOutput resellerNutxoAddr)
                            (fromIntegral nameUtxoSats)
                            (OutPoint (fromJust $ hexToTxHash $ DT.pack $ opTxHash op) (fromIntegral $ opIndex op))
                            (setForkIdFlag sigHashAll)
                            Nothing
                fundingUtxos <- getFundingUtxos nexaAddr sessionKey resellerFundAddrString rqFundingSats Nothing
                return (rootNameInput, fundingUtxos, True)
            else do
                fundingUtxos <- getFundingUtxos nexaAddr sessionKey resellerFundAddrString rqFundingSats Nothing
                (nameRoot, remFundInput) <- makeProducer (init nameArr) fundingUtxos producerRoot op
                return (nameRoot, remFundInput, False)
    debug lg $
        LG.msg $
        "xGetPartiallySignedAllegoryTx got nameRoot: " <> (show nameRoot) <> ", remFundInput: " <> (show remFundInput)
    --
    let inputs =
            ((\si -> TxIn (sigInputOP si) (encodeOutputBS $ sigInputScript si) 0xFFFFFFFF) <$> (nameRoot : remFundInput)) ++
            ((\(x, s) ->
                  TxIn
                      (OutPoint (fromString $ opTxHash x) (fromIntegral $ opIndex x))
                      (fromJust $ decodeHex s)
                      0xFFFFFFFF) <$>
             ((\ip -> (fst ip, DT.pack "")) <$> payips))
        sigInputs = nameRoot : remFundInput
        remFunding =
            (foldl (\p q -> p + (fromIntegral $ sigInputValue q)) 0 remFundInput) - (getFundingUtxoValue nameUtxoSats)
        inputValues =
            [nameUtxoSats] ++
            (case remFunding of
                 0 -> []
                 f -> [(foldl (\p q -> p + (fromIntegral $ sigInputValue q)) 0 remFundInput)]) ++
            (snd <$> payips)
        paySats = NC.defaultPriceSats nodeCfg
        allegoryFeeSatsCreate = NC.feeSatsCreate nodeCfg
        allegoryFeeSatsTransfer = NC.feeSatsTransfer nodeCfg
        totalEffectiveInputSats = sum $ snd $ unzip $ payips
        outputs =
            (if existed
                 then let action =
                              if isProducer
                                  then (ProducerAction
                                            (Index 0)
                                            (ProducerOutput (Index 1) (Just $ Endpoint "XokenP2P" buyerUri))
                                            Nothing
                                            [])
                                  else (OwnerAction
                                            (Index 0)
                                            (OwnerOutput (Index 1) (Just $ Endpoint "XokenP2P" buyerUri))
                                            [])
                          opRetScript = frameOpReturn $ C.toStrict $ serialise $ Allegory 1 nameArr action
                          changeSats = totalEffectiveInputSats - (paySats + allegoryFeeSatsTransfer)
                       in [ TxOut 0 opRetScript
                          , TxOut (fromIntegral nameUtxoSats) ownerScriptPubKey
                          , TxOut (fromIntegral changeSats) changeScriptPubKey
                          , TxOut (fromIntegral paySats) resellerPaymentScriptPubKey
                          ]
                 else let action =
                              ProducerAction
                                  (Index 0)
                                  (ProducerOutput (Index 1) (Just $ Endpoint "XokenP2P" resellerUri))
                                  Nothing $
                              OwnerExtension
                                  (OwnerOutput (Index 2) (Just $ Endpoint "XokenP2P" buyerUri))
                                  (last nameArr) :
                              if isProducer
                                  then [ ProducerExtension
                                             (ProducerOutput (Index 3) (Just $ Endpoint "XokenP2P" buyerUri))
                                             (last nameArr)
                                       ]
                                  else []
                          opRetScript = frameOpReturn $ C.toStrict $ serialise $ Allegory 1 (init nameArr) action
                          changeSats =
                              totalEffectiveInputSats -
                              (paySats +
                               (if isProducer
                                    then 2
                                    else 1) *
                               allegoryFeeSatsCreate)
                       in [ TxOut 0 opRetScript
                          , TxOut (fromIntegral nameUtxoSats) resellerNutxoScriptPubKey
                          , TxOut (fromIntegral nameUtxoSats) ownerScriptPubKey
                          ] ++
                          (if isProducer
                               then [TxOut (fromIntegral nameUtxoSats) ownerScriptPubKey]
                               else []) ++
                          [ TxOut (fromIntegral changeSats) changeScriptPubKey
                          , TxOut (fromIntegral paySats) resellerPaymentScriptPubKey
                          ]) ++
            case (length remFundInput) of
                0 -> []
                _ -> [TxOut (fromIntegral remFunding) resellerFundScriptPubKey]
    let psatx = Tx 1 inputs outputs 0
    case signTx net psatx sigInputs [nameSecKey, fundSecKey] of
        Right tx -> return $ BSL.toStrict $ A.encode $ createTx' tx inputValues
        Left err -> do
            liftIO $ print $ "error occured while signing tx: " <> show err
            return BC.empty

makeProducer ::
       (MonadHttp m, HasResellerEnv env m, MonadIO m)
    => [Int]
    -> [SigInput]
    -> [Int]
    -> OutPoint'
    -> m (SigInput, [SigInput])
makeProducer name gotFundInputs fromRoot rootOutpoint
    | name == fromRoot = do
        lg <- getLogger
        nodeCfg <- nodeConfig <$> getBitcoinP2P
        nameSecKey <- nameUtxoSecKey <$> getAllegory
        let nameUtxoSats = NC.nameUtxoSatoshis nodeCfg
            nameAddr = pubKeyAddr $ derivePubKeyI $ wrapSecKey True $ nameSecKey
            nameScript = addressToScriptBS nameAddr
            nextNameInput =
                SigInput
                    (addressToOutput nameAddr)
                    (fromIntegral nameUtxoSats)
                    (OutPoint
                         (fromJust $ hexToTxHash $ DT.pack $ opTxHash rootOutpoint)
                         (fromIntegral $ opIndex rootOutpoint))
                    (setForkIdFlag sigHashAll)
                    Nothing
        debug lg $ LG.msg $ "makeProducer: returned root node as producer: " <> (show nextNameInput)
        return (nextNameInput, gotFundInputs)
    | otherwise = do
        lg <- getLogger
        debug lg $ LG.msg $ "makeProducer: called for name: " <> (chr <$> name)
        (nameInput, fundInput) <- makeProducer (init name) gotFundInputs fromRoot rootOutpoint
        bp2pEnv <- getBitcoinP2P
        nodeCfg <- nodeConfig <$> getBitcoinP2P
        sessionKey <- nexaSessionKey <$> getNexaEnv
        nameSecKey <- nameUtxoSecKey <$> getAllegory
        fundSecKey <- fundUtxoSecKey <$> getAllegory
        nexaAddr <- (\nc -> return $ NC.nexaListenIP nc <> ":" <> (show $ NC.nexaListenPort nc)) $ (nodeConfig bp2pEnv)
        debug lg $ LG.msg $ "makeProducer: " <> (chr <$> name) <> ": got keys & nexa endpoint " <> (show nexaAddr)
        let net = NC.bitcoinNetwork nodeCfg
            nameString = chr <$> name
            nameUtxoSats = NC.nameUtxoSatoshis nodeCfg
            nameAddr = pubKeyAddr $ derivePubKeyI $ wrapSecKey True $ nameSecKey
            nameScript = addressToScriptBS nameAddr
            fundAddr = pubKeyAddr $ derivePubKeyI $ wrapSecKey True $ fundSecKey
            fundScript = addressToScriptBS fundAddr
            resellerUri = NC.resellerUri nodeCfg
            remFunding =
                (foldl (\p q -> p + (fromIntegral $ sigInputValue q)) 0 fundInput) -
                (fromIntegral (getFundingUtxoValue nameUtxoSats))
        debug lg $
            LG.msg $
            "makeProducer: " <> nameString <> ": got addresses, scripts, remaining funding: " <> (show remFunding)
        let ins =
                ((\si -> TxIn (sigInputOP si) nameScript 0xFFFFFFFF) $ nameInput) :
                ((\si -> TxIn (sigInputOP si) fundScript 0xFFFFFFFF) <$> fundInput)
        debug lg $ LG.msg $ "makeProducer: " <> nameString <> ": got inputs: " <> (show ins)
        let al =
                Allegory
                    1
                    (init name)
                    (ProducerAction
                         (Index 0)
                         (ProducerOutput (Index 1) (Just $ Endpoint "XokenP2P" resellerUri))
                         Nothing
                         [ (ProducerExtension
                                (ProducerOutput (Index 2) (Just $ Endpoint "XokenP2P" resellerUri))
                                (last name))
                         , (OwnerExtension (OwnerOutput (Index 3) (Just $ Endpoint "XokenP2P" resellerUri)) (last name))
                         ])
        let opRetScript = frameOpReturn $ C.toStrict $ serialise al
        let outs =
                [TxOut 0 opRetScript] ++
                L.map (\_ -> TxOut (fromIntegral nameUtxoSats) nameScript) [1, 2, 3] ++ [TxOut remFunding fundScript]
        debug lg $ LG.msg $ "makeProducer: " <> nameString <> ": got outputs: " <> (show outs)
        let sigInputs = nameInput : fundInput
        let psaTx = Tx 1 ins outs 0
        case signTx net psaTx sigInputs $ [nameSecKey] ++ (take (length fundInput) $ repeat fundSecKey) of
            Right tx -> do
                debug lg $ LG.msg $ "makeProducer: " <> nameString <> ": signed txn: " <> (show tx)
                let nextFundInputs =
                        case remFunding of
                            0 -> []
                            r' ->
                                [ SigInput
                                      (addressToOutput fundAddr)
                                      (fromIntegral r')
                                      (OutPoint (txHash tx) (fromIntegral 4))
                                      (setForkIdFlag sigHashAll)
                                      Nothing
                                ]
                    nextNameInput =
                        SigInput
                            (addressToOutput nameAddr)
                            (fromIntegral nameUtxoSats)
                            (OutPoint (txHash tx) (fromIntegral 2))
                            (setForkIdFlag sigHashAll)
                            Nothing
                debug lg $
                    LG.msg $
                    "makeProducer: for name " <> nameString <> " relaying rawTx (" <>
                    (DT.unpack . txHashToHex $ txHash tx) <>
                    "): " <>
                    (BC.unpack . B16.encode . Data.Serialize.encode $ tx)
                res <- LE.try $ liftIO $ relayTx nexaAddr sessionKey (Data.Serialize.encode tx)
                case res of
                    Left (e :: SomeException) -> do
                        debug lg $ LG.msg $ "makeProducer: " <> nameString <> ": failed to relay txn: " <> (show e)
                        throw e
                    Right (RelayTxResponse res) ->
                        case res of
                            True -> debug lg $ LG.msg $ "makeProducer: relayed transaction: " <> nameString
                            False -> do
                                err lg $ LG.msg $ show "[ERROR] Failed to relay intermediate transaction!"
                                throw KeyValueDBLookupException
                return (nextNameInput, nextFundInputs)
            Left e -> do
                err lg $ LG.msg $ "Error: Failed to sign interim producer transaction: " <> (show e)
                throw KeyValueDBLookupException

getFundingUtxoValue :: Int -> Int
getFundingUtxoValue ns = (ns * 2) + 5000

createTx' :: Tx -> [Int] -> Tx'
createTx' (Tx version inputs outs locktime) inputValues =
    Tx' {txVersion = version, txIn = fmap func $ Prelude.zip inputs inputValues, txOut = outs, txLockTime = locktime}
  where
    func (TxIn prevOut scriptIn txInSeq, val) =
        TxIn' {prevOutput = prevOut, scriptInput = scriptIn, txInSequence = txInSeq, value = fromIntegral val}
