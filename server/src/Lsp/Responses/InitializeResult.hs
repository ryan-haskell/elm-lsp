module Lsp.Responses.InitializeResult (
    InitializeResult, create,
    encode
) where

import qualified Data.Maybe
import qualified Json.Encode
import qualified Lsp.Id
import qualified Lsp.Server
import qualified Lsp.SemanticTokensOptions


create :: InitializeResult
create =
    InitializeResult
        (ServerInfo "elmLsp" Lsp.Server.version)
        (Capabilities
            { semanticTokensProvider = Just Lsp.SemanticTokensOptions.create
            }
        )


data InitializeResult = InitializeResult
    { serverInfo :: ServerInfo
    , capabilities :: Capabilities
    }


encode :: InitializeResult -> Json.Encode.Value
encode result =
    Json.Encode.object
        [ ( "capabilities", encodeCapabilities (capabilities result) )
        , ( "serverInfo", encodeServerInfo (serverInfo result) )
        ]


data Capabilities = Capabilities
    { semanticTokensProvider :: Maybe Lsp.SemanticTokensOptions.SemanticTokensOptions
    }


encodeCapabilities :: Capabilities -> Json.Encode.Value
encodeCapabilities capabilities =
    Json.Encode.object
        (Data.Maybe.catMaybes 
            [ case semanticTokensProvider capabilities of
                Just options ->
                    Just ( "semanticTokensProvider", Lsp.SemanticTokensOptions.encode options )

                Nothing ->
                    Nothing
            , Just
                ( "textDocumentSync"
                , Json.Encode.object
                    [ ( "openClose", Json.Encode.bool True )
                    , ( "change", Json.Encode.int 1 ) -- FULL
                    ]
                )
            ]
        )


data ServerInfo = ServerInfo
    { name :: String
    , version :: String
    }


encodeServerInfo :: ServerInfo -> Json.Encode.Value
encodeServerInfo (ServerInfo name version) =
    Json.Encode.object
        [ ( "name", Json.Encode.string name)
        , ( "version", Json.Encode.string version)
        ]
