module Lsp.Capabilities.TextDocument.DidChangeParams (
    DidChangeParams(..)
    , decoder
    , toFsPath
) where

import Data.Function ((&))
import qualified Json.Decode
import qualified Lsp.DocumentUri


data DidChangeParams = DidChangeParams
    { uri :: Lsp.DocumentUri.DocumentUri
    , text :: String
    }


toFsPath :: DidChangeParams -> String
toFsPath params =
    Lsp.DocumentUri.toFsPath (uri params)


decoder :: Json.Decode.Decoder DidChangeParams
decoder =
    Json.Decode.field "contentChanges" (Json.Decode.list (Json.Decode.field "text" Json.Decode.string))
        & Json.Decode.andThen (\contentChanges ->
            case contentChanges of
                [] ->
                    Json.Decode.fail "Missing content changes"
                
                (text_ : []) ->
                    Json.Decode.object (\uri -> DidChangeParams uri text_)
                        & Json.Decode.withField "textDocument" (Json.Decode.field "uri" Lsp.DocumentUri.decoder)
                
                _ ->
                    Json.Decode.fail "Too many content changes"
        )
    
