{-# LANGUAGE DeriveGeneric #-}
module Main where

import Prelude hiding (id)
import Data.Aeson
import Data.ByteString.Char8 (pack)
import GHC.Generics
import System.Environment
import System.IO
import Data.List (isPrefixOf)
import Data.Char (isSpace)

-- ENTRYPOINT

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--version"] -> putStrLn "Version 1.0.0"
        _ -> runLspServer

-- STATE OF THE LSP SERVER

data State
    = State Int Step
    deriving(Show)

data Step
    = ExpectingContentLength
    | ExpectingBlankLine ContentLength
    | ExpectingJson ContentLength
    deriving(Show)

type ContentLength = Int

initialState :: State
initialState =
    State 1 ExpectingContentLength


runLspServer :: IO ()
runLspServer =do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    -- Open a log file for writing
    withFile "/Users/ryan/code/ryan-haskell/elm-lsp/server/input.log" WriteMode $ \handle -> do
        -- hSetBuffering handle LineBuffering
        clearFileContents handle
        loop initialState handle

loop :: State -> Handle -> IO ()
loop (State num step) handle = 
    let 
        continueWithStep :: Step -> IO ()
        continueWithStep newStep = loop (State (num + 1) newStep) handle
    in
    do
        line <- getLine

        case step of
            ExpectingContentLength ->
                if startsWith "Content-Length: " line then
                    let
                        trimmedLine :: String
                        trimmedLine =
                            drop (length "Content-Length: ") line
                    in
                    case fromStringToInt trimmedLine of
                        Just contentLength ->
                            continueWithStep (ExpectingBlankLine contentLength)

                        Nothing -> do
                            hPutStr handle $ "ERR: Content-Length was not an integer!" <> "\n"
                            hPutStr handle $ "- Line: " <> line <> "\n"
                            hPutStr handle $ "- Trim: " <> trimmedLine <> "\n"

                else
                    hPutStr handle $ "ERR: Expected Content-Length prefix!" <> "\n"
            
            ExpectingBlankLine contentLength ->
                if trim line == "" then
                    continueWithStep (ExpectingJson contentLength)
                else
                    hPutStr handle $ "ERR: Expected blank line!" <> "\n"

            ExpectingJson contentLength ->
                if contentLength == length line then
                    case (fromJson line) of
                        Just request -> do
                            hPutStr handle $ "REQUEST: " <> show request <> "\n"
                            hFlush handle
                            case method request of
                                "initialize" ->
                                    respond handle $ "{\"jsonrpc\":\"2.0\",\"id\":" <> show (id request) <> ",\"result\":{\"capabilities\":{}}}"
                                _ ->
                                    respond handle $ "{\"jsonrpc\":\"2.0\",\"id\":" <> show (id request) <> ",\"error\":{\"code\":-32601,\"message\":\"TODO\"}}"
                            continueWithStep ExpectingContentLength

                        Nothing ->
                            hPutStr handle $ "ERR: Our JSON decoder failed!" <> "\n"
                else
                    hPutStr handle $ "ERR: Content length did not match JSON string length!" <> "\n"
            


data Request = Request { id :: Int, jsonrpc :: String, method :: String }
    deriving (Show, Generic)

instance FromJSON Request

fromJson :: String -> Maybe Request
fromJson rawJson = decodeStrict (pack rawJson) :: Maybe Request

respond :: Handle -> String -> IO ()
respond handle json =
    let

        jsonRpcMessage :: String
        jsonRpcMessage =
            "Content-Length: " <> show (length json) <> "\r\n\r\n" <> json

    in do
        putStr jsonRpcMessage
        hFlush stdout
        hPutStr handle jsonRpcMessage
        hFlush handle

-- UTILS

startsWith :: String -> String -> Bool
startsWith prefix str = isPrefixOf prefix str

isDivisibleBy3 :: Int -> Bool
isDivisibleBy3 n = n `mod` 3 == 0

-- Trim leading and trailing whitespace
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

fromStringToInt :: String -> Maybe Int
fromStringToInt str = case reads (trim str) of
    [(x, "")] -> Just x
    _         -> Nothing

clearFileContents :: Handle -> IO ()
clearFileContents handle = do
    -- Move the file pointer to the beginning of the file
    hSeek handle AbsoluteSeek 0
    -- Truncate the file to zero length
    hSetFileSize handle 0