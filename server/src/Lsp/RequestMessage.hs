module Lsp.RequestMessage (RequestMessage(..), decoder) where

import Data.Function ((&))
import qualified Json.Decode
import qualified Json.Encode
import qualified Lsp.Id

-- TODO: "If a server or client receives a request starting with ‘$/’ it must error the request with error code MethodNotFound (e.g. -32601)."
--       https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#dollarRequests

data RequestMessage = RequestMessage
    { jsonrpc :: String
    , id :: Lsp.Id.Id
    , method :: String
    , params :: Json.Decode.Value
    }


decoder :: Json.Decode.Decoder RequestMessage
decoder =
    Json.Decode.object RequestMessage
        & Json.Decode.withField "jsonrpc" Json.Decode.string
        & Json.Decode.withField "id" Lsp.Id.decoder
        & Json.Decode.withField "method" Json.Decode.string
        & Json.Decode.with 
            (Json.Decode.oneOf
                [ Json.Decode.field "params" Json.Decode.value
                , Json.Decode.succeed Json.Encode.null
                ]
            )
