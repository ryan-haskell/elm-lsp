module Lsp.Responses.InitializeResponse (
    InitializeResponse, create,
    encode
) where

import qualified Json.Encode
import qualified Json.Encode
import qualified Lsp.Server
import qualified Lsp.Id


data InitializeResponse = InitializeResponse
    { id :: Lsp.Id.Id
    , result :: InitializeResult
    }


create :: Lsp.Id.Id -> InitializeResponse
create id =
    InitializeResponse
        id
        (InitializeResult
            (Capabilities {})
            (ServerInfo "elmLsp" Lsp.Server.version)
        )


encode :: InitializeResponse -> Json.Encode.Value
encode (InitializeResponse id result) =
    Json.Encode.object
        [ ( "jsonrpc", Json.Encode.string "2.0" )
        , ( "id", Lsp.Id.encode id )
        , ( "result", encodeInitializeResult result )
        ]


data InitializeResult = InitializeResult
    { capabilities :: Capabilities
    , serverInfo :: ServerInfo
    }


encodeInitializeResult :: InitializeResult -> Json.Encode.Value
encodeInitializeResult (InitializeResult capabilities serverInfo) =
    Json.Encode.object
        [ ( "capabilities", encodeCapabilities capabilities )
        , ( "serverInfo", encodeServerInfo serverInfo )
        ]


data Capabilities = Capabilities
    {}


encodeCapabilities :: Capabilities -> Json.Encode.Value
encodeCapabilities Capabilities =
    Json.Encode.object []


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
