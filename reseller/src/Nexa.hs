{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Nexa where

import Control.Exception
import Control.Monad.IO.Unlift
import Data.Aeson
import Data.Int
import Data.List
import Data.Maybe
import qualified Data.Text as DT
import Network.HTTP.Client
import Network.Xoken.Address
import Network.Xoken.Script
import Network.Xoken.Transaction
import Nexa.Auth
import Nexa.Constants
import Nexa.Types
import NodeConfig
import Reseller.Env
import System.Logger as LG

getProducer :: (MonadUnliftIO m) => String -> SessionKey -> [Int] -> Bool -> m NameOutpointResponse
getProducer addr sk name isProducer = do
    response <- liftIO $ nexaReq NameOutpoint (encode $ NameOutpointRequest name isProducer) addr (Just sk)
    case decode (responseBody response) :: Maybe NameOutpointResponse of
        Nothing -> throw ResponseParseException
        Just nameOutpointResponse -> return nameOutpointResponse

getUtxoByAddress :: (MonadIO m) => String -> SessionKey -> String -> m [AddressOutputs]
getUtxoByAddress nexaAddr sk addr = do
    response <- liftIO $ nexaGetReq (utxosByAddressRequest nexaAddr addr 10 Nothing) (Just sk)
    case decode (responseBody response) :: Maybe GetUtxosByAddressResponse of
        Nothing -> throw ResponseParseException
        Just resp -> return $ utxos resp

showSI :: SigInput -> String
showSI (SigInput _ v (OutPoint h i) _ _) = show $ ((h, i), v)

getFundingUtxos ::
       (HasResellerEnv env m, MonadIO m) => String -> SessionKey -> String -> Int -> Maybe String -> m [SigInput]
getFundingUtxos nexaAddr sk addr rqMileage cursor = do
    lg <- getLogger
    net <- (return . bitcoinNetwork) =<< (nodeConfig <$> getBitcoinP2P)
    let req = utxosByAddressRequest nexaAddr addr 10 cursor
    debug lg $ LG.msg $ "<getFundingUtxos> address = " <> addr <> ": sent Nexa request '" <> req <> "'"
    response <- liftIO $ nexaGetReq req (Just sk)
    case decode (responseBody response) :: Maybe GetUtxosByAddressResponse of
        Nothing -> do
            err lg $ LG.msg $ "Error: Failed to parse Nexa response for request '" <> req <> "'"
            throw ResponseParseException
        Just resp -> do
            debug lg $ LG.msg $ "<getFundingUtxos> address = " <> addr <> ": got Nexa response: " <> (show resp)
            scriptPubKey <-
                case stringToAddr net (DT.pack addr) of
                    Nothing -> do
                        err lg $
                            LG.msg $ show "Error: Failed to decode '" <> addr <> "' as a " <> (show net) <> " address"
                        throw ResponseParseException
                    Just a' -> return $ addressToOutput a'
            let requiredSats = rqMileage * 2500
            debug lg $ LG.msg $ "<getFundingUtxos> address = " <> addr <> ": required sats: " <> (show requiredSats)
            let fundingUtxos =
                    sortOn sigInputValue $
                    (\ao ->
                         SigInput
                             scriptPubKey
                             (fromIntegral $ value ao)
                             (OutPoint
                                  (fromJust $ hexToTxHash $ DT.pack $ outputTxHash ao)
                                  (fromIntegral $ outputIndex ao))
                             (setForkIdFlag sigHashAll)
                             Nothing) <$>
                    utxos resp
            debug lg $
                LG.msg $
                "<getFundingUtxos> address = " <> addr <> ": got funding utxos: " <> (show $ showSI <$> fundingUtxos)
            let pickUtxos li rqSats = f [] li rqSats
                  where
                    f li [] _ = li
                    f li (x:xs) s
                        | (foldl (\p q -> p + (fromIntegral $ sigInputValue q)) 0 li) <= s = f (x : li) xs s
                        | otherwise = li
                selectedUtxos = pickUtxos fundingUtxos requiredSats
                gotSats = foldl (\p q -> p + (fromIntegral $ sigInputValue q)) 0 selectedUtxos
            debug lg $
                LG.msg $
                "<getFundingUtxos> address = " <> addr <> ": picked utxos to meet value req: " <>
                (show $ showSI <$> selectedUtxos) <>
                ", got total sats value: " <>
                (show gotSats) <>
                ", required value: " <>
                (show requiredSats)
            if (length selectedUtxos == length fundingUtxos) && (gotSats < requiredSats)
                then do
                    debug lg $ LG.msg $ "<getFundingUtxos> address = " <> addr <> ": requesting next page of utxos..."
                    moreUtxos <- getFundingUtxos nexaAddr sk addr (rqMileage - gotSats) (nextCursor resp)
                    return $ selectedUtxos ++ moreUtxos
                else return selectedUtxos
