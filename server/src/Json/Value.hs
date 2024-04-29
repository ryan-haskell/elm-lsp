module Json.Value (Value(..), unwrap) where

import qualified Data.Aeson.Types as Value

data Value = Value Value.Value


unwrap :: Value -> Value.Value
unwrap (Value val) =
    val