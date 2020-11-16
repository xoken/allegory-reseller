{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

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
import qualified Data.ByteString.Base16 as B16 (decode, encode)
import Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as C
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
import NodeConfig
import Prelude
import Reseller.Common
import Reseller.Env
import Reseller.HTTP.Types
import System.Logger as LG
import Xoken

xGetPartiallySignedAllegoryTx ::
       (MonadHttp m, HasResellerEnv env m, MonadIO m)
    => NodeConfig
    -> [(OutPoint', Int)]
    -> ([Int], Bool)
    -> (String)
    -> (String)
    -> m (BC.ByteString)
xGetPartiallySignedAllegoryTx nodeCnf payips (nameArr, isProducer) owner change = do
    lg <- getLogger
    bp2pEnv <- getBitcoinP2P
    nodeCfg <- nodeConfig <$> getBitcoinP2P
    sessionKey <- nexaSessionKey <$> getNexaEnv
    nameSecKey <- nameUtxoSecKey <$> getAllegory
    fundSecKey <- fundUtxoSecKey <$> getAllegory
    nexaAddr <- (\nc -> return $ nexaListenIP nc <> ":" <> (show $ nexaListenPort nc)) $ (nodeConfig bp2pEnv)
    producer <- liftIO $ getProducer nexaAddr sessionKey nameArr isProducer
    let producerRoot = forName producer
    let rqMileage = (length nameArr) - (length producerRoot)
    let scr = script producer
    let op = outPoint producer
    let net = bitcoinNetwork nodeCfg
    let nameUtxoSats = nameUtxoSatoshis nodeCfg
    let nameAddr = pubKeyAddr $ derivePubKeyI $ wrapSecKey False $ nameSecKey
    let nameScript = addressToScriptBS nameAddr
    let nameAddr' =
            case addrToString net nameAddr of
                Nothing -> ""
                Just t -> DT.unpack t
    let fundAddr = pubKeyAddr $ derivePubKeyI $ wrapSecKey False $ fundSecKey
    let fundScript = addressToScriptBS fundAddr
    let fundAddr' =
            case addrToString net fundAddr of
                Nothing -> ""
                Just t -> DT.unpack t
    (nameRoot, remFundInput, existed) <-
        if (producerRoot == init nameArr) || (producerRoot == nameArr)
            then do
                let rootNameInput =
                        SigInput
                            (addressToOutput nameAddr)
                            (fromIntegral nameUtxoSats)
                            (OutPoint (fromJust $ hexToTxHash $ DT.pack $ opTxHash op) (fromIntegral $ opIndex op))
                            (setForkIdFlag sigHashAll)
                            Nothing
                return (rootNameInput, [], True)
            else do
                fundingUtxos <- getFundingUtxos nexaAddr sessionKey fundAddr' rqMileage Nothing
                (nameRoot, remFundInput) <- makeProducer (init nameArr) fundingUtxos producerRoot op
                return (nameRoot, remFundInput, False)
    --
    let paySats = defaultPriceSats nodeCfg
    let allegoryFeeSatsCreate = feeSatsCreate nodeCfg
    let allegoryFeeSatsTransfer = feeSatsTransfer nodeCfg
    let net = bitcoinNetwork nodeCfg
    let totalEffectiveInputSats = sum $ snd $ unzip $ payips
    let ins =
            ((\si -> TxIn (sigInputOP si) (encodeOutputBS $ sigInputScript si) 0xFFFFFFFF) <$> (nameRoot : remFundInput)) ++
            ((\(x, s) ->
                  TxIn
                      (OutPoint (fromString $ opTxHash x) (fromIntegral $ opIndex x))
                      (fromJust $ decodeHex s)
                      0xFFFFFFFF) <$>
             ((\ip -> (fst ip, DT.pack "")) <$> payips))
    let sigInputs = nameRoot : remFundInput
    let outs =
            if existed
                then if isProducer
                         then do
                             let al =
                                     Allegory
                                         1
                                         (init nameArr)
                                         (ProducerAction
                                              (Index 0)
                                              (ProducerOutput (Index 1) (Just $ Endpoint "XokenP2P" "someuri_1"))
                                              Nothing
                                              [])
                             let opRetScript = frameOpReturn $ C.toStrict $ serialise al
                             let prAddr = pubKeyAddr $ derivePubKeyI $ wrapSecKey False $ nameSecKey
                             let prScript = addressToScriptBS prAddr
                             let payScript = addressToScriptBS prAddr
                             let changeSats = totalEffectiveInputSats - (paySats + allegoryFeeSatsCreate)
                             [TxOut 0 opRetScript] ++
                                 (L.map
                                      (\x -> do
                                           let addr =
                                                   case stringToAddr net (DT.pack $ fst x) of
                                                       Just a -> a
                                                       Nothing -> throw KeyValueDBLookupException
                                           let script = addressToScriptBS addr
                                           TxOut (fromIntegral $ snd x) script)
                                      [(owner, (fromIntegral $ nameUtxoSats)), (change, changeSats)]) ++
                                 [TxOut ((fromIntegral paySats) :: Word64) payScript]
                         else do
                             let al =
                                     Allegory
                                         1
                                         (nameArr)
                                         (OwnerAction
                                              (Index 0)
                                              (OwnerOutput (Index 1) (Just $ Endpoint "XokenP2P" "someuri_1"))
                                              [ ProxyProvider
                                                    "AllPay"
                                                    "Public"
                                                    (Endpoint "XokenP2P" "someuri_2")
                                                    (Registration "addrCommit" "utxoCommit" "signature" 876543)
                                              ])
                             let opRetScript = frameOpReturn $ C.toStrict $ serialise al
                             let payAddr = pubKeyAddr $ derivePubKeyI $ wrapSecKey False $ nameSecKey
                             let payScript = addressToScriptBS payAddr
                             let changeSats = totalEffectiveInputSats - (paySats + allegoryFeeSatsTransfer)
                             [TxOut 0 opRetScript] ++
                                 (L.map
                                      (\x -> do
                                           let addr =
                                                   case stringToAddr net (DT.pack $ fst x) of
                                                       Just a -> a
                                                       Nothing -> throw KeyValueDBLookupException
                                           let script = addressToScriptBS addr
                                           TxOut (fromIntegral $ snd x) script)
                                      [(owner, (fromIntegral $ nameUtxoSats)), (change, changeSats)]) ++
                                 [TxOut ((fromIntegral paySats) :: Word64) payScript]
                else do
                    let al =
                            Allegory
                                1
                                (init nameArr)
                                (ProducerAction
                                     (Index 0)
                                     (ProducerOutput (Index 1) (Just $ Endpoint "XokenP2P" "someuri_1"))
                                     Nothing
                                     [ OwnerExtension
                                           (OwnerOutput (Index 2) (Just $ Endpoint "XokenP2P" "someuri_3"))
                                           (last nameArr)
                                     ])
                    let opRetScript = frameOpReturn $ C.toStrict $ serialise al
                    let prAddr = pubKeyAddr $ derivePubKeyI $ wrapSecKey False $ nameSecKey
                    let prScript = addressToScriptBS prAddr
                    let payScript = addressToScriptBS prAddr
                    let changeSats = totalEffectiveInputSats - (paySats + allegoryFeeSatsCreate)
                    [TxOut 0 opRetScript] ++
                        (L.map
                             (\x -> do
                                  let addr =
                                          case stringToAddr net (DT.pack $ fst x) of
                                              Just a -> a
                                              Nothing -> throw KeyValueDBLookupException
                                  let script = addressToScriptBS addr
                                  TxOut (fromIntegral $ snd x) script)
                             [(owner, (fromIntegral $ nameUtxoSats)), (change, changeSats)]) ++
                        [TxOut ((fromIntegral paySats) :: Word64) payScript]
    let psatx = Tx 1 ins outs 0
    case signTx net psatx sigInputs [nameSecKey, fundSecKey] of
        Right tx -> return $ BSL.toStrict $ A.encode tx
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
        nodeCfg <- nodeConfig <$> getBitcoinP2P
        nameSecKey <- nameUtxoSecKey <$> getAllegory
        let nameUtxoSats = nameUtxoSatoshis nodeCfg
            nameAddr = pubKeyAddr $ derivePubKeyI $ wrapSecKey False $ nameSecKey
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
        return (nextNameInput, gotFundInputs)
    | otherwise = do
        (nameInput, fundInput) <- makeProducer (init name) gotFundInputs fromRoot rootOutpoint
        lg <- getLogger
        bp2pEnv <- getBitcoinP2P
        nodeCfg <- nodeConfig <$> getBitcoinP2P
        sessionKey <- nexaSessionKey <$> getNexaEnv
        nameSecKey <- nameUtxoSecKey <$> getAllegory
        fundSecKey <- fundUtxoSecKey <$> getAllegory
        let net = bitcoinNetwork nodeCfg
            nameUtxoSats = nameUtxoSatoshis nodeCfg
            nameAddr = pubKeyAddr $ derivePubKeyI $ wrapSecKey False $ nameSecKey
            nameScript = addressToScriptBS nameAddr
            fundAddr = pubKeyAddr $ derivePubKeyI $ wrapSecKey False $ fundSecKey
            fundScript = addressToScriptBS fundAddr
            remFunding = (foldl (\p q -> p + (fromIntegral $ sigInputValue q)) 0 fundInput) - 2500
        let ins =
                ((\si -> TxIn (sigInputOP si) nameScript 0xFFFFFFFF) $ nameInput) :
                ((\si -> TxIn (sigInputOP si) fundScript 0xFFFFFFFF) <$> fundInput)
        let al =
                Allegory
                    1
                    (init name)
                    (ProducerAction
                         (Index 0)
                         (ProducerOutput (Index 1) (Just $ Endpoint "XokenP2P" "someuri_1"))
                         Nothing
                         [ (ProducerExtension
                                (ProducerOutput (Index 2) (Just $ Endpoint "XokenP2P" "someuri_2"))
                                (last name))
                         , (OwnerExtension (OwnerOutput (Index 3) (Just $ Endpoint "XokenP2P" "someuri_3")) (last name))
                         ])
        let opRetScript = frameOpReturn $ C.toStrict $ serialise al
        let outs =
                [TxOut 0 opRetScript] ++
                L.map (\_ -> TxOut (fromIntegral nameUtxoSats) nameScript) [1, 2, 3] ++ [TxOut 2500 fundScript]
        let sigInputs = nameInput : fundInput
        let psaTx = Tx 1 ins outs 0
        case signTx net psaTx sigInputs $ [nameSecKey] ++ (take (length fundInput) $ repeat fundSecKey) of
            Right tx -> do
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
                xRelayTx (nodeCfg) (Data.Serialize.encode tx)
                return (nextNameInput, nextFundInputs)
            Left e -> do
                err lg $ LG.msg $ "Error: Failed to sign interim producer transaction: " <> (show e)
                throw KeyValueDBLookupException

xRelayTx :: (MonadHttp m, MonadIO m, MonadBaseControl IO m) => NodeConfig -> BC.ByteString -> m (Bool)
xRelayTx nodeCnf rawTx = do
    resp <-
        req
            POST
            (https (DT.pack $ nexaListenIP nodeCnf))
            (ReqBodyJson (RelayTx rawTx))
            bsResponse
            (port $ fromEnum $ nexaListenPort nodeCnf)
    case eitherDecodeStrict $ responseBody resp of
        Right (RespRelayTx b) -> undefined
        Left err -> undefined
