module Lsp.NotificationMessage (NotificationMessage(..), decoder) where

import Data.Function ((&))
import qualified Json.Decode
import qualified Json.Encode


data NotificationMessage = NotificationMessage
    { jsonrpc :: String
    , method :: String
    , params :: Json.Decode.Value
    }


decoder :: Json.Decode.Decoder NotificationMessage
decoder =
    Json.Decode.object NotificationMessage
        & Json.Decode.withField "jsonrpc" Json.Decode.string
        & Json.Decode.withField "method" Json.Decode.string
        & Json.Decode.with
            (Json.Decode.oneOf
                [ Json.Decode.field "params" Json.Decode.value
                , Json.Decode.succeed Json.Encode.null
                ]
            )
