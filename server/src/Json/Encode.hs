module Json.Encode (
    Value
    , object
    , string, int, bool, null
    , list
    , toString
) where

import Prelude hiding (null)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Value
import qualified Data.Aeson.Encoding as Encode
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text
import qualified Data.Scientific
import qualified Data.Vector
import Json.Value


object :: [(String, Value)] -> Value
object pairs =
    Value $ 
        Value.Object (KeyMap.fromList (map (\(k, (Value v)) -> (Key.fromString k, v)) pairs))


string :: String -> Value
string text =
    Value (Value.String (Data.Text.pack text))


int :: Int -> Value
int num =
    Value (Value.Number (Data.Scientific.scientific (toInteger num) 0))


bool :: Bool -> Value
bool val =
    Value (Value.Bool val)


null :: Value
null =
    Value Value.Null


list :: (item -> Value) -> [item] -> Value
list encodeItem items =
    Value (Value.Array (Data.Vector.fromList (map (Json.Value.unwrap . encodeItem) items)))


-- CONVERTING TO TEXT


toString :: Value -> String
toString (Value value) =
    LBS.unpack (Encode.encodingToLazyByteString (Encode.value value))

