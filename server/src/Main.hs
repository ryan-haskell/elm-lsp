module Main where

import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.C.String (peekCStringLen)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Aeson ((.=))
import qualified Json.Encode as Json
import qualified System.IO as IO
import Data.Char (isSpace)
import Text.Read (readMaybe)

-- Main loop to listen on stdin
main :: IO ()
main = 
    waitUntilStartingLspServer
    
waitUntilStartingLspServer :: IO ()
waitUntilStartingLspServer = do
    IO.withFile "/Users/ryan/code/ryan-haskell/elm-lsp/server/logfile.txt" IO.WriteMode $ \logfile -> do
        IO.hSetBuffering logfile IO.LineBuffering
        IO.hPutStrLn logfile ("Elm LSP is running...")
        isReady <- IO.hWaitForInput IO.stdin 1000
        if isReady then do
            IO.hPutStrLn logfile ("Ready!")
            loop logfile (ExpectingContentLength "")
        else do
            IO.hPutStrLn logfile ("Not ready!")



-- THIS WORKS WITH VS CODE, but doesn't actually read stdin
sendFakeInitResponse :: IO ()
sendFakeInitResponse = do
    threadDelay (1 * 1000000)
    sendInitializationResponse
    threadDelay (3 * 1000000)


-- THIS WORKS fine with `elm-lsp < request.txt`, 
-- but not with actual VS Code!
startLspServer :: IO ()
startLspServer =
    IO.withFile "/Users/ryan/code/ryan-haskell/elm-lsp/server/logfile.txt" IO.WriteMode $ \logfile -> do
        IO.hSetBuffering logfile IO.LineBuffering
        IO.hPutStrLn logfile "Elm LSP is running..."
        loop logfile (ExpectingContentLength "")




data State
    = ExpectingContentLength String
    | CollectingJsonString Int


loop :: IO.Handle -> State -> IO ()
loop logfile state =
    case state of
        CollectingJsonString contentLength -> do
            -- LOG
            IO.hPutStrLn logfile "Collecting JSON..."
            -- Open stdin in binary mode
            IO.hSetBinaryMode IO.stdin True
            -- Allocate a buffer to store the bytes
            buffer <- mallocBytes contentLength
            -- Read the next contentLength bytes from stdin into the buffer
            bytesRead <- IO.hGetBuf IO.stdin buffer contentLength
            -- Convert the buffer to a String
            jsonString <- peekCStringLen (buffer, bytesRead)
            -- Print, delay, then exit
            IO.hPutStrLn logfile ("JSON: " <> jsonString)
            sendInitializationResponse
            threadDelay (3 * 1000000)


        ExpectingContentLength textSoFar -> do
            if endsWith "\r\n\r\n" textSoFar then
                case parseContentLength textSoFar of
                    Just contentLength ->
                        loop logfile (CollectingJsonString contentLength)

                    Nothing -> do
                        IO.hPutStrLn logfile ("Failed to parse content length from: " <> textSoFar)

            else do
                IO.hPutStrLn logfile ("TextSoFar: " <> textSoFar)
                -- Read lines one at a time from stdin
                char <- IO.hGetChar IO.stdin
                loop logfile (ExpectingContentLength (textSoFar <> [char]))


parseContentLength :: String -> Maybe Int
parseContentLength text =
    readMaybe (trim (drop (length "Content-Length: ") text))


sendInitializationResponse :: IO ()
sendInitializationResponse =
    let
        response :: InitializeResponse
        response =
            InitializeResponse
                0
                (InitializeResult
                    (Capabilities {})
                    (ServerInfo "elmLsp" "1.0.0")
                )
    in
    sendLspResponse encodeInitializeResponse response


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


-- STRING UTILS

endsWith :: String -> String -> Bool
endsWith suffix str =
    let
        endingOfStr = drop (length str - length suffix) str
    in
    suffix == endingOfStr

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace


fromStringToInt :: String -> Maybe Int
fromStringToInt = readMaybe