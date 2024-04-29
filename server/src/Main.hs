module Main where

import Data.Char (isSpace)
import Data.Function ((&))
import Text.Read (readMaybe)
import qualified Data.ByteString
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import qualified Json.Decode
import qualified Json.Encode
import qualified System.IO as IO
import qualified System.Environment
import qualified Lsp.Capabilities.SemanticTokens
import qualified Lsp.Capabilities.TextDocument.DidChangeParams
import qualified Lsp.Capabilities.TextDocument.DidCloseParams
import qualified Lsp.Capabilities.TextDocument.DidOpenParams
import qualified Lsp.Id
import qualified Lsp.Message
import qualified Lsp.NotificationMessage
import qualified Lsp.RequestMessage
import qualified Lsp.ResponseMessage
import qualified Lsp.Responses.InitializeResult
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
    IO.withBinaryFile "/Users/ryan/code/ryan-haskell/elm-lsp/server/logfile.txt" IO.WriteMode $ \logfile -> do
        IO.hSetBuffering IO.stdin IO.NoBuffering
        IO.hSetBuffering logfile IO.NoBuffering
        IO.hPutStrLn logfile "Elm LSP is running..."
        loop logfile (State Map.empty (ExpectingContentLength ""))


data State = State
    { openTextDocuments :: Map.Map String String
    , stdio :: StdioState
    }


updateStdioState :: StdioState -> State -> State
updateStdioState newStdio state =
    State
        (openTextDocuments state)
        newStdio


data StdioState
    = ExpectingContentLength String
    | CollectingJsonString Int


loop :: IO.Handle -> State -> IO ()
loop logfile state =
    case stdio state of
        CollectingJsonString contentLength -> do
            -- Open stdin in binary mode
            byteString <- Data.ByteString.hGet IO.stdin contentLength
            -- Convert the buffer to a String
            let jsonString = B8.unpack byteString
            -- Log the JSON for debugging
            IO.hPutStrLn logfile ("\nJSON: " <> jsonString)
            -- Send response to LSP client
            newState <- sendResponseToJson logfile jsonString state
            -- Continue to listen for the next message
            loop logfile (updateStdioState (ExpectingContentLength "") newState)


        ExpectingContentLength textSoFar -> do
            -- The Content-Length part is done when we get "\r\n\r\n"
            if endsWith "\r\n\r\n" textSoFar then
                case parseContentLength textSoFar of
                    Just contentLength ->
                        -- Start to collect characters for JSON payload
                        loop logfile (updateStdioState (CollectingJsonString contentLength) state)

                    Nothing -> do
                        -- Exit if something goes wrong
                        IO.hPutStrLn logfile ("Failed to parse Content-Length from: " <> textSoFar)

            else do
                char <- IO.hGetChar IO.stdin
                -- Keep collecting characters for "Content-Length" header
                loop logfile (updateStdioState (ExpectingContentLength (textSoFar <> [char])) state)


parseContentLength :: String -> Maybe Int
parseContentLength text =
    readMaybe (trim (drop (length "Content-Length: ") text))


sendResponseToJson :: IO.Handle -> String -> State -> IO State
sendResponseToJson logfile jsonString state =
    case Json.Decode.decodeString jsonString Lsp.Message.decoder of
        Right (Lsp.Message.Request request) ->
            handleLspRequestMessage logfile request state

        Right (Lsp.Message.Notification request) ->
            handleLspNotificationMessage logfile request state

        Left problem -> do
            IO.hPutStrLn logfile (Json.Decode.fromProblemToString problem)
            return state


handleLspRequestMessage :: IO.Handle -> Lsp.RequestMessage.RequestMessage -> State -> IO State
handleLspRequestMessage logfile request state = do
    IO.hPutStrLn logfile ("REQUEST: " <> Lsp.RequestMessage.method request)
    case Lsp.RequestMessage.method request of
        "initialize" -> do
            _ <- Lsp.Responses.InitializeResult.create
                & Lsp.Responses.InitializeResult.encode
                & sendLspResponse logfile request
            return state

        "textDocument/semanticTokens/full" ->
            let
                paramsJson = Lsp.RequestMessage.params request
            in
            case Json.Decode.decodeValue paramsJson Lsp.Capabilities.SemanticTokens.decoder of
                Left problem -> do
                    IO.hPutStrLn logfile (Json.Decode.fromProblemToString problem)
                    return state

                Right params -> do
                    result <- Lsp.Capabilities.SemanticTokens.handle logfile (openTextDocuments state) params
                    sendLspResponse logfile request (Lsp.Capabilities.SemanticTokens.encode result)
                    return state

        _ ->
            return state


handleLspNotificationMessage :: IO.Handle -> Lsp.NotificationMessage.NotificationMessage -> State -> IO State
handleLspNotificationMessage logfile notification state = do
    IO.hPutStrLn logfile ("NOTIFICATION: " <> Lsp.NotificationMessage.method notification)
    case Lsp.NotificationMessage.method notification of
        "textDocument/didOpen" ->
            let
                result :: Either Json.Decode.Problem Lsp.Capabilities.TextDocument.DidOpenParams.DidOpenParams
                result =
                    Json.Decode.decodeValue
                        (Lsp.NotificationMessage.params notification)
                        Lsp.Capabilities.TextDocument.DidOpenParams.decoder
            in
            case result of
                Left problem -> do
                    IO.hPutStrLn logfile (Json.Decode.fromProblemToString problem)
                    return state

                Right params -> do
                    let fsPath = Lsp.Capabilities.TextDocument.DidOpenParams.toFsPath params
                    IO.hPutStrLn logfile ("FS PATH: " <> fsPath)
                    let text = Lsp.Capabilities.TextDocument.DidOpenParams.text params
                    let newState = insertTextDocument fsPath text state
                    return newState

        "textDocument/didClose" ->
            let
                result :: Either Json.Decode.Problem Lsp.Capabilities.TextDocument.DidCloseParams.DidCloseParams
                result =
                    Json.Decode.decodeValue
                        (Lsp.NotificationMessage.params notification)
                        Lsp.Capabilities.TextDocument.DidCloseParams.decoder
            in
            case result of
                Left problem -> do
                    IO.hPutStrLn logfile (Json.Decode.fromProblemToString problem)
                    return state

                Right params -> do
                    let fsPath = Lsp.Capabilities.TextDocument.DidCloseParams.toFsPath params
                    IO.hPutStrLn logfile ("FS PATH: " <> fsPath)
                    let newState = removeTextDocument fsPath state
                    return newState

        "textDocument/didChange" ->
            let
                result :: Either Json.Decode.Problem Lsp.Capabilities.TextDocument.DidChangeParams.DidChangeParams
                result =
                    Json.Decode.decodeValue
                        (Lsp.NotificationMessage.params notification)
                        Lsp.Capabilities.TextDocument.DidChangeParams.decoder
            in
            case result of
                Left problem -> do
                    IO.hPutStrLn logfile (Json.Decode.fromProblemToString problem)
                    return state

                Right params -> do
                    let fsPath = Lsp.Capabilities.TextDocument.DidChangeParams.toFsPath params
                    IO.hPutStrLn logfile ("FS PATH: " <> fsPath)
                    let text = Lsp.Capabilities.TextDocument.DidChangeParams.text params
                    let newState = insertTextDocument fsPath text state
                    return newState

        _ ->
            return state


sendLspResponse :: IO.Handle -> Lsp.RequestMessage.RequestMessage -> Json.Encode.Value -> IO ()
sendLspResponse logfile request resultJson = do
    let id = Lsp.RequestMessage.id request
    let responseMessage = Lsp.ResponseMessage.create id resultJson
    let json = Lsp.ResponseMessage.encode responseMessage
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


-- STATE

insertTextDocument :: String -> String -> State -> State
insertTextDocument fsPath text (State map s) =
    State (Map.insert fsPath text map) s

removeTextDocument :: String -> State -> State
removeTextDocument fsPath (State map s) =
    State (Map.delete fsPath map) s

-- STRING UTILS


endsWith :: String -> String -> Bool
endsWith suffix str =
    let
        endingOfStr = drop (length str - length suffix) str
    in
    suffix == endingOfStr


trim :: String -> String
trim =
    let
        f = reverse . dropWhile isSpace
    in
    f . f


fromStringToInt :: String -> Maybe Int
fromStringToInt =
    readMaybe