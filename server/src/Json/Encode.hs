module Json.Encode (
    Value
    , object
    , string, int
    , toString
) where

import qualified Data.Aeson.Encoding as Encode
import qualified Data.ByteString.Lazy.Char8 as LBS

data Value = Value Encode.Encoding

object :: [(String, Value)] -> Value
object pairs =
    Value $
        Encode.dict
            Encode.string
            unwrapValue
            (\f initVal pairs -> foldr (\(k, v) acc -> f k v acc) initVal pairs)
            pairs


string :: String -> Value
string text =
    Value (Encode.string text)


int :: Int -> Value
int num =
    Value (Encode.int num)


-- CONVERTING TO TEXT

toString :: Value -> String
toString (Value encoding) =
    LBS.unpack (Encode.encodingToLazyByteString encoding)


-- UTILS

unwrapValue :: Value -> Encode.Encoding
unwrapValue (Value encoding) =
    encoding