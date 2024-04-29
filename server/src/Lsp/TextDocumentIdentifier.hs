module Lsp.TextDocumentIdentifier (
    TextDocumentIdentifier
    , decoder
    , toFsPath
) where

import Data.Function ((&))
import qualified Json.Decode
import qualified Lsp.DocumentUri


data TextDocumentIdentifier = TextDocumentIdentifier
    { uri :: Lsp.DocumentUri.DocumentUri
    }


decoder :: Json.Decode.Decoder TextDocumentIdentifier
decoder =
    Json.Decode.object TextDocumentIdentifier
        & Json.Decode.withField "uri" Lsp.DocumentUri.decoder


toFsPath :: TextDocumentIdentifier -> String
toFsPath identifier =
    Lsp.DocumentUri.toFsPath (uri identifier)