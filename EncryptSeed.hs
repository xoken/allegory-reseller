import Crypto.TripleSec as TS
import Data.ByteString.Base64 as B64
import Data.ByteString.Char8 as BC

encryptSeed passphrase nutxoSecKey = B64.encode <$> TS.encryptIO (BC.pack passphrase) (BC.pack nutxoSecKey)
