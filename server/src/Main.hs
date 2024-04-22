module Main where

import qualified Json.Encode as Json
import Data.Aeson ((.=))
import qualified System.IO as IO

import Control.Concurrent

main :: IO ()
main =
    let
        response :: InitializeResponse
        response =
            InitializeResponse
                0
                (InitializeResult
                    (Capabilities {})
                    (ServerInfo "elmLsp" "1.0.0")
                )
    in do
        _ <- threadDelay 500000
        sendLspResponse encodeInitializeResponse response
        threadDelay 4000000


sendLspResponse :: (response -> Json.Value) -> response -> IO ()
sendLspResponse toJson response = do
    IO.hPutStr IO.stdout (toRpcString (toJson response))
    IO.hFlush IO.stdout


toRpcString :: Json.Value -> String
toRpcString json =
    let
        jsonString :: String
        jsonString =
            Json.toString json
    in
    "Content-Length: " <> show (length jsonString) <> "\r\n\r\n" <> jsonString

data InitializeResponse = InitializeResponse
    { id :: Int
    , result :: InitializeResult
    }

encodeInitializeResponse :: InitializeResponse -> Json.Value
encodeInitializeResponse (InitializeResponse id result) =
    Json.object
        [ ( "jsonrpc", Json.string "2.0" )
        , ( "id", Json.int id )
        , ( "result", encodeInitializeResult result )
        ]


data InitializeResult = InitializeResult
    { capabilities :: Capabilities
    , serverInfo :: ServerInfo
    }

encodeInitializeResult :: InitializeResult -> Json.Value
encodeInitializeResult (InitializeResult capabilities serverInfo) =
    Json.object
        [ ( "capabilities", encodeCapabilities capabilities )
        , ( "serverInfo", encodeServerInfo serverInfo )
        ]


data Capabilities = Capabilities
    {}

encodeCapabilities :: Capabilities -> Json.Value
encodeCapabilities Capabilities =
    Json.object []


data ServerInfo = ServerInfo
    { name :: String
    , version :: String
    }

encodeServerInfo :: ServerInfo -> Json.Value
encodeServerInfo (ServerInfo name version) =
    Json.object
        [ ( "name", Json.string name)
        , ( "version", Json.string version)
        ]

-- instance Json.ToJSON ServerInfo where
--     toJSON (ServerInfo name version) = Json.object ["name" .= name, "version" .= version]

-- string :: String -> Json.Value
-- string str = Json.String (Json.toJSON str)

-- int :: Int -> Json.Value
-- int num = Json.Number (fromIntegral num)