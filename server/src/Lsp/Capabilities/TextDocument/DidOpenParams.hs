module Lsp.Capabilities.TextDocument.DidOpenParams (
    DidOpenParams(..)
    , decoder
    , toFsPath
) where

import Data.Function ((&))
import qualified Json.Decode
import qualified Lsp.DocumentUri


data DidOpenParams = DidOpenParams
    { uri :: Lsp.DocumentUri.DocumentUri
    , text :: String
    }


toFsPath :: DidOpenParams -> String
toFsPath params =
    Lsp.DocumentUri.toFsPath (uri params)


decoder :: Json.Decode.Decoder DidOpenParams
decoder =
    Json.Decode.field "textDocument"
        (Json.Decode.object DidOpenParams 
            & Json.Decode.withField "uri" Lsp.DocumentUri.decoder
            & Json.Decode.withField "text" Json.Decode.string
        )
