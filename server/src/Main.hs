module Main where

import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.C.String (peekCStringLen)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Aeson ((.=))
import Data.Char (isSpace)
import Data.Function ((&))
import Text.Read (readMaybe)
import qualified Json.Decode
import qualified Json.Encode
import qualified System.IO as IO
import qualified System.Environment
import qualified Lsp.Id
import qualified Lsp.Message
import qualified Lsp.NotificationMessage
import qualified Lsp.RequestMessage
import qualified Lsp.Responses.InitializeResponse
import qualified Lsp.Server


main :: IO ()
main = do
    args <- System.Environment.getArgs
    case args of
        ["--version"] ->
            IO.putStrLn Lsp.Server.version

        _ ->
            startLspServer
    

-- Main loop to listen on stdin
startLspServer :: IO ()
startLspServer =
    IO.withFile "/Users/ryan/code/ryan-haskell/elm-lsp/server/logfile.txt" IO.WriteMode $ \logfile -> do
        IO.hSetBuffering IO.stdin IO.NoBuffering
        IO.hSetBuffering logfile IO.NoBuffering
        IO.hPutStrLn logfile "Elm LSP is running..."
        loop logfile (ExpectingContentLength "")


data State
    = ExpectingContentLength String
    | CollectingJsonString Int


-- TODO: "Use functions like https://hackage.haskell.org/package/bytestring-0.12.1.0/docs/Data-ByteString.html#v:getContents to skip the Foreign modules"
-- TODO: "Prefer https://hackage.haskell.org/package/base-4.19.1.0/docs/System-IO.html#v:withBinaryFile to avoid any silly encoding/newline behavior"
loop :: IO.Handle -> State -> IO ()
loop logfile state =
    case state of
        CollectingJsonString contentLength -> do
            -- Open stdin in binary mode
            IO.hSetBinaryMode IO.stdin True
            -- Allocate a buffer to store the bytes
            buffer <- mallocBytes contentLength
            -- Read the next contentLength bytes from stdin into the buffer
            bytesRead <- IO.hGetBuf IO.stdin buffer contentLength
            -- Convert the buffer to a String
            jsonString <- peekCStringLen (buffer, bytesRead)
            -- Log the JSON for debugging
            IO.hPutStrLn logfile ("\nJSON: " <> jsonString)
            -- Send response to LSP client
            sendResponseToJson logfile jsonString
            -- Continue to listen for the next message
            loop logfile (ExpectingContentLength "")


        ExpectingContentLength textSoFar -> do
            -- The Content-Length part is done when we get "\r\n\r\n"
            if endsWith "\r\n\r\n" textSoFar then
                case parseContentLength textSoFar of
                    Just contentLength ->
                        -- Start to collect characters for JSON payload
                        loop logfile (CollectingJsonString contentLength)

                    Nothing -> do
                        -- Exit if something goes wrong
                        IO.hPutStrLn logfile ("Failed to parse Content-Length from: " <> textSoFar)

            else do
                char <- IO.hGetChar IO.stdin
                -- Keep collecting characters for "Content-Length" header
                loop logfile (ExpectingContentLength (textSoFar <> [char]))


parseContentLength :: String -> Maybe Int
parseContentLength text =
    readMaybe (trim (drop (length "Content-Length: ") text))


sendResponseToJson :: IO.Handle -> String -> IO ()
sendResponseToJson logfile jsonString =
    case Json.Decode.decode jsonString Lsp.Message.decoder of
        Right (Lsp.Message.Request request) ->
            handleLspRequestMessage logfile request

        Right (Lsp.Message.Notification request) ->
            handleLspNotificationMessage logfile request

        Left problem ->
            IO.hPutStrLn logfile (Json.Decode.fromProblemToString problem)


handleLspRequestMessage :: IO.Handle -> Lsp.RequestMessage.RequestMessage -> IO ()
handleLspRequestMessage logfile request = do
    IO.hPutStrLn logfile ("REQUEST: " <> Lsp.RequestMessage.method request)
    case Lsp.RequestMessage.method request of
        "initialize" -> sendInitializationResponse logfile request
        _ -> return ()


handleLspNotificationMessage :: IO.Handle -> Lsp.NotificationMessage.NotificationMessage -> IO ()
handleLspNotificationMessage logfile request = do
    IO.hPutStrLn logfile ("NOTIFICATION: " <> Lsp.NotificationMessage.method request)
    case Lsp.NotificationMessage.method request of
        _ -> return ()


sendInitializationResponse :: IO.Handle -> Lsp.RequestMessage.RequestMessage -> IO ()
sendInitializationResponse logfile request =
    Lsp.RequestMessage.id request
        & Lsp.Responses.InitializeResponse.create
        & sendLspResponse logfile Lsp.Responses.InitializeResponse.encode


sendLspResponse :: IO.Handle -> (response -> Json.Encode.Value) -> response -> IO ()
sendLspResponse logfile toJson response = do
    let json = toJson response
    IO.hPutStr IO.stdout (toRpcString json)
    IO.hFlush IO.stdout
    IO.hPutStrLn logfile ("RESPONSE: " <> Json.Encode.toString json)


toRpcString :: Json.Encode.Value -> String
toRpcString json =
    let
        jsonString :: String
        jsonString =
            Json.Encode.toString json
    in
    "Content-Length: " <> show (length jsonString) <> "\r\n\r\n" <> jsonString


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