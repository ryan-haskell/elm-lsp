module Lsp.Capabilities.TextDocument.DidCloseParams (
    DidCloseParams(..)
    , decoder
    , toFsPath
) where

import Data.Function ((&))
import qualified Json.Decode
import qualified Lsp.DocumentUri


data DidCloseParams = DidCloseParams
    { uri :: Lsp.DocumentUri.DocumentUri
    }


toFsPath :: DidCloseParams -> String
toFsPath params =
    Lsp.DocumentUri.toFsPath (uri params)


decoder :: Json.Decode.Decoder DidCloseParams
decoder =
    Json.Decode.field "textDocument"
        (Json.Decode.object DidCloseParams 
            & Json.Decode.withField "uri" Lsp.DocumentUri.decoder
        )
