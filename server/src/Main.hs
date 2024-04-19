module Main where

import System.Environment
import System.IO

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--version"] -> putStrLn "Version 1.0.0"
        _ -> runLspServer



runLspServer :: IO ()
runLspServer =do
    -- Open a log file for writing
    withFile "/Users/ryan/code/ryan-haskell/elm-lsp/server/input.log" WriteMode $ \handle -> do
        hSetBuffering handle NoBuffering
        loop handle

loop :: Handle -> IO ()
loop handle = do
    -- Read a line from stdin
    line <- getChar
    _ <- putStr [line]
    -- Write the line to the log file
    _ <- hPutStr handle [line]
    _ <- hFlush handle
    -- Continue reading lines from stdin
    loop handle