module Parser.AST (Declaration(..), Module(..), parse) where

import Data.Function ((&))
import Parser.Core (Parser, (|=), (|.))
import qualified Parser.Core as Parser


parse :: String -> [Declaration]
parse source =
    case Parser.run declarationParser source of
        Left _ -> []
        Right declaration -> [declaration]


data Declaration
    = ModuleDeclaration Module
    deriving (Show)


declarationParser :: Parser Declaration
declarationParser =
    Parser.oneOf
        [ Parser.map ModuleDeclaration moduleParser
        ]


data Module = Module
    { moduleKeyword :: Parser.Located ()
    , moduleName :: Parser.Located String
    , exposingKeyword :: Maybe (Parser.Located ())
    } deriving (Show)


moduleParser :: Parser Module
moduleParser =
    Parser.succeed Module
        |= Parser.located (Parser.keyword "module")
        |. Parser.spaces
        |= Parser.located (
            Parser.chompWhile (\c -> not (Parser.isWhitespace c))
                & Parser.getChompedString
        )
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.located (Parser.keyword "exposing")
                & Parser.map Just
            , Parser.succeed Nothing
            ]