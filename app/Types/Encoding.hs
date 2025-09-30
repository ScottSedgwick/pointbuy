module Types.Encoding ( decode, encode ) where

import qualified Codec.Compression.GZip  as Z
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString.Base64  as B64
import qualified Data.Serialize          as DS
import qualified Data.Text               as TL
import qualified Data.Text.Encoding      as TL
import           Miso                    ( MisoString, fromMisoString, ms )
import qualified Network.URI.Encode      as NU

encode :: DS.Serialize a => a -> MisoString
encode = ms . NU.encode . TL.unpack . TL.decodeUtf8 . B64.encode . lazyToStrict . Z.compress . DS.encodeLazy

strictToLazy :: BS.ByteString -> BL.ByteString
strictToLazy = BL.pack . BS.unpack

lazyToStrict :: BL.ByteString -> BS.ByteString
lazyToStrict = BS.pack . BL.unpack

decode :: DS.Serialize a => String -> Maybe a
decode s = 
  case B64.decode b64 of
    Left _ -> Nothing
    Right bs -> decompressDecode bs
  where
    b64 = TL.encodeUtf8 $ TL.pack $ NU.decode s

decompressDecode :: DS.Serialize a => BS.ByteString -> Maybe a
decompressDecode s = 
  case DS.decodeLazy bs of
    Left _ -> Nothing
    Right a -> Just a
  where
    bs = Z.decompress $ strictToLazy s