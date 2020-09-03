module Reseller.Common where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.Aeson.Encoding as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as CL
import Data.Serialize as S
import Data.Text (Text)
import Data.Word
import Network.Xoken.Crypto.Hash
import Network.Xoken.Util
import Prelude
import System.Random

data ResellerException =
    KeyValueDBLookupException
    deriving (Show)

instance Exception ResellerException

maskAfter :: Int -> String -> String
maskAfter n skey = (\x -> take n x ++ fmap (const '*') (drop n x)) skey

generateSessionKey :: IO (Text)
generateSessionKey = do
    g <- liftIO $ newStdGen
    let seed = show $ fst (random g :: (Word64, StdGen))
        sdb = B64.encode $ C.pack $ seed
    return $ encodeHex ((S.encode $ sha256 $ B.reverse sdb))

encodeResp :: ToJSON a => Bool -> a -> CL.ByteString
encodeResp True = AP.encodePretty
encodeResp False = A.encode

encodeStrict :: ToJSON a => a -> B.ByteString
encodeStrict = BL.toStrict . A.encode
