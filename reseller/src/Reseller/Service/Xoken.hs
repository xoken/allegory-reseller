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
    allSecKey <- allegorySecretKey <$> getAllegory
    nexaAddr <- (\nc -> return $ xokenListenIP nc <> ":" <> (show $ xokenListenPort nc)) $ (nodeConfig bp2pEnv)
    (producerRoot, scr, op) <- liftIO $ getProducer nexaAddr sessionKey nameArr
    (nameip, existed) <-
        if (producerRoot == init nameArr) || (producerRoot == nameArr)
            then return ((op, DT.pack scr), True)
            else do
                gotProducer <- makeProducer (init nameArr) producerRoot op
                return (gotProducer, False)
    --
    let paySats = defaultSathosi nodeCfg
    let anutxos = nameUtxoSatoshis nodeCfg
    let allegoryFeeSatsCreate = feeSatsCreate nodeCfg
    let allegoryFeeSatsTransfer = feeSatsTransfer nodeCfg
    let net = bitcoinNetwork nodeCfg
    let totalEffectiveInputSats = sum $ snd $ unzip $ payips
    let ins =
            L.map
                (\(x, s) ->
                     TxIn (OutPoint (fromString $ opTxHash x) (fromIntegral $ opIndex x)) (fromJust $ decodeHex s) 0)
                ([nameip] ++ ((\ip -> (fst ip, DT.pack "")) <$> payips))
    sigInputs <-
        mapM
            (\(x, s) -> do
                 case (decodeOutputBS ((fst . B16.decode) (E.encodeUtf8 s))) of
                     Left e -> do
                         throw KeyValueDBLookupException
                     Right scr -> do
                         return $
                             SigInput
                                 scr
                                 (fromIntegral $ anutxos)
                                 (OutPoint (fromString $ opTxHash x) (fromIntegral $ opIndex x))
                                 (setForkIdFlag sigHashAll)
                                 Nothing)
            [nameip]
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
                             let prAddr = pubKeyAddr $ derivePubKeyI $ wrapSecKey False $ allSecKey
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
                                      [(owner, (fromIntegral $ anutxos)), (change, changeSats)]) ++
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
                             let payAddr = pubKeyAddr $ derivePubKeyI $ wrapSecKey False $ allSecKey
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
                                      [(owner, (fromIntegral $ anutxos)), (change, changeSats)]) ++
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
                    let prAddr = pubKeyAddr $ derivePubKeyI $ wrapSecKey False $ allSecKey
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
                             [(owner, (fromIntegral $ anutxos)), (change, changeSats)]) ++
                        [TxOut ((fromIntegral paySats) :: Word64) payScript]
    let psatx = Tx 1 ins outs 0
    case signTx net psatx sigInputs [allSecKey] of
        Right tx -> return $ BSL.toStrict $ A.encode tx
        Left err -> do
            liftIO $ print $ "error occured while signing tx: " <> show err
            return BC.empty

makeProducer :: (MonadHttp m, HasResellerEnv env m, MonadIO m) => [Int] -> [Int] -> OutPoint' -> m (OutPoint', DT.Text)
makeProducer name fromRoot rootOutpoint
    | name == fromRoot = do return (rootOutpoint, "")
    | otherwise = do
        nameInput <- makeProducer (init name) fromRoot rootOutpoint
        lg <- getLogger
        bp2pEnv <- getBitcoinP2P
        nodeCfg <- nodeConfig <$> getBitcoinP2P
        sessionKey <- nexaSessionKey <$> getNexaEnv
        allSecKey <- allegorySecretKey <$> getAllegory
        let anutxos = nameUtxoSatoshis nodeCfg
        let net = bitcoinNetwork nodeCfg
        let prAddr = pubKeyAddr $ derivePubKeyI $ wrapSecKey False $ allSecKey
        let prScript = addressToScriptBS prAddr
        let addr' =
                case addrToString net prAddr of
                    Nothing -> ""
                    Just t -> DT.unpack t
        nexaAddr <- (\nc -> return $ xokenListenIP nc <> ":" <> (show $ xokenListenPort nc)) $ (nodeConfig bp2pEnv)
        -- TODO: make and commit interim transaction
        let ins' =
                L.map
                    (\(x, s) ->
                         TxIn (OutPoint (fromString $ opTxHash x) (fromIntegral $ opIndex x)) (fromJust $ decodeHex s) 0)
                    ([nameInput])
        fundingUtxo <- getUtxoByAddress nexaAddr sessionKey addr'
        let (ins, fval) =
                ( ins' ++
                  [ TxIn
                        (OutPoint (fromString $ AO.outputTxHash fundingUtxo) (fromIntegral $ AO.outputIndex fundingUtxo))
                        prScript
                        0xFFFFFFFF
                  ]
                , AO.value fundingUtxo)
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
        let outs = [TxOut 0 opRetScript] ++ L.map (\_ -> TxOut (fromIntegral anutxos) prScript) [1, 2, 3]
        let sigInputs =
                [ SigInput
                      (addressToOutput prAddr)
                      (fromIntegral anutxos)
                      (prevOutput $ head ins)
                      (setForkIdFlag sigHashAll)
                      Nothing
                , SigInput
                      (addressToOutput prAddr)
                      (fromIntegral fval)
                      (prevOutput $ ins !! 1)
                      (setForkIdFlag sigHashAll)
                      Nothing
                ]
        let psaTx = Tx 1 ins outs 0
        case signTx net psaTx sigInputs [allSecKey, allSecKey] of
            Right tx -> do
                xRelayTx (nodeCfg) (Data.Serialize.encode tx)
                return (txHash tx, prScript)
            Left err -> do
                liftIO $ putStrLn $ "error occured while signing transaction: " <> show err
                throw KeyValueDBLookupException
        return nameInput

xRelayTx :: (MonadHttp m, MonadIO m, MonadBaseControl IO m) => NodeConfig -> BC.ByteString -> m (Bool)
xRelayTx nodeCnf rawTx = do
    resp <-
        req
            POST
            (https (DT.pack $ xokenListenIP nodeCnf))
            (ReqBodyJson (RelayTx rawTx))
            bsResponse
            (port $ fromEnum $ xokenListenPort nodeCnf)
    case eitherDecodeStrict $ responseBody resp of
        Right (RespRelayTx b) -> undefined
        Left err -> undefined
