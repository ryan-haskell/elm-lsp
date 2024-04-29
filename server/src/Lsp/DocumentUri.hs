module Lsp.DocumentUri (
    DocumentUri, toFsPath
    , decoder
    , fromFilepath
) where

import Data.Function ((&))
import qualified Json.Decode


data DocumentUri
    = FromFile String


fromFilepath :: String -> DocumentUri
fromFilepath fsPath =
    FromFile fsPath


toFsPath :: DocumentUri -> String
toFsPath uri =
    case uri of
        FromFile fsPath -> fsPath


decoder :: Json.Decode.Decoder DocumentUri
decoder =
    Json.Decode.string 
        & Json.Decode.andThen (\rawUri ->
            case rawUri of
                'f' : 'i' : 'l': 'e' : ':' : '/': '/' : fsPath ->
                    Json.Decode.succeed (FromFile fsPath)
                
                _ ->
                    Json.Decode.fail ("Unrecognized URI: " <> rawUri)
        )
