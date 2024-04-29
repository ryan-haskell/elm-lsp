module Lsp.ResponseMessage (
    ResponseMessage, create
    , encode
) where

import qualified Data.Maybe
import qualified Json.Encode
import qualified Lsp.Server
import qualified Lsp.SemanticTokensOptions
import qualified Lsp.Id


data ResponseMessage = ResponseMessage
    { id :: Lsp.Id.Id
    , result :: Json.Encode.Value
    }


create :: Lsp.Id.Id -> Json.Encode.Value -> ResponseMessage
create id resultJson =
    ResponseMessage id resultJson
        

encode :: ResponseMessage -> Json.Encode.Value
encode (ResponseMessage id result) =
    Json.Encode.object
        [ ( "jsonrpc", Json.Encode.string "2.0" )
        , ( "id", Lsp.Id.encode id )
        , ( "result", result )
        ]
