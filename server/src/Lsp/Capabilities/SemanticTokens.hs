module Lsp.Capabilities.SemanticTokens (
    Params, decoder
    , Result, encode, handle
    , tokenTypes, tokenModifiers
) where

import Data.Function ((&))
import qualified Data.ByteString
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import qualified Json.Decode
import qualified Json.Encode
import qualified Lsp.TextDocumentIdentifier
import qualified System.IO as IO
import qualified Parser.AST
import qualified Parser.Core



-- REQUEST PARAMS


data Params = Params
    { textDocument :: Lsp.TextDocumentIdentifier.TextDocumentIdentifier
    }


decoder :: Json.Decode.Decoder Params
decoder =
    Json.Decode.object Params
        & Json.Decode.withField "textDocument" Lsp.TextDocumentIdentifier.decoder



-- RESPONSE


data Result = Result
    { data_ :: [SemanticToken]
    }


encode :: Result -> Json.Encode.Value
encode result =
    Json.Encode.object
        [ ( "data", encodeSemanticTokens (data_ result) )
        ]


handle :: IO.Handle -> Map.Map String String -> Params -> IO Result
handle logfile textDocumentMap params = 
    let
        toTokens :: Parser.AST.Declaration -> [SemanticToken]
        toTokens declaration =
            case declaration of
                Parser.AST.ModuleDeclaration (Parser.AST.Module (Parser.Core.Located (m1Line,m1Char,m1Len) _) (Parser.Core.Located (m2Line,m2Char,m2Len) _) maybeExposing) ->
                    case maybeExposing of
                        Just (Parser.Core.Located (eLine,eChar,eLen) _) ->
                            [ SemanticToken m1Line m1Char m1Len Keyword []
                            , SemanticToken m2Line m2Char m2Len Namespace [Declaration]
                            , SemanticToken eLine eChar eLen Keyword []
                            ]
                        
                        Nothing ->
                            [ SemanticToken m1Line m1Char m1Len Keyword []
                            , SemanticToken m2Line m2Char m2Len Namespace [Declaration]
                            ]
            
        fsPath :: String
        fsPath =
            Lsp.TextDocumentIdentifier.toFsPath (textDocument params)
    in do
        IO.hPutStrLn logfile ("FS PATH: " <> fsPath)
        case Map.lookup fsPath textDocumentMap of 
            Just sourceCode -> do
                let declarations = Parser.AST.parse sourceCode
                IO.hPutStrLn logfile ("DECLARATIONS: " <> show declarations)
                return (Result (concatMap toTokens declarations))

            Nothing -> do
                IO.hPutStrLn logfile ("SOURCE CODE NOT FOUND!")
                return (Result [])



-- SEMANTIC TOKENS


data SemanticToken = SemanticToken
    { line :: Int
    , start :: Int
    , length_ :: Int
    , tokenType :: TokenType
    , modifiers :: [Modifier]
    }


encodeSemanticTokens :: [SemanticToken] -> Json.Encode.Value
encodeSemanticTokens tokens =
    Json.Encode.list Json.Encode.int
        (toSemanticTokenInts tokens ( 0, 0 ) [])


toSemanticTokenInts :: [SemanticToken] -> (Int, Int) -> [Int] -> [Int]
toSemanticTokenInts tokens ( lastLine, lastStart ) ints =
    case tokens of
        [] -> ints
        token : remainingTokens ->
            let
                newLine = line token - 1
                newStart = start token - 1

                deltaLine = newLine - lastLine
                deltaStart = if newLine == lastLine then newStart - lastStart else newStart
                semanticLength = length_ token
                tokenInt = fromTokenToInt (tokenType token)
                modifierInt = sum (map fromModifierToInt (modifiers token))
            in
            toSemanticTokenInts remainingTokens ( newLine, newStart )
                (ints <> [ deltaLine, deltaStart, semanticLength, tokenInt, modifierInt ])



-- TOKEN TYPES


data TokenType
    = Namespace
	| Type
	| Class
	| Enum
	| Interface
	| Struct
	| TypeParameter
	| Parameter
	| Variable
	| Property
	| EnumMember
	| Event
	| Function
	| Method
	| Macro
	| Keyword
	| Modifier
	| Comment
	| String_
	| Number
	| Regexp
	| Operator


tokenTypes :: [String]
tokenTypes =
    [ "namespace"
    , "type"
    , "class"
    , "enum"
    , "interface"
    , "struct"
    , "typeParameter"
    , "parameter"
    , "variable"
    , "property"
    , "enumMember"
    , "event"
    , "function"
    , "method"
    , "macro"
    , "keyword"
    , "modifier"
    , "comment"
    , "string"
    , "number"
    , "regexp"
    , "operator"
    ]


fromTokenToInt :: TokenType -> Int
fromTokenToInt tokenType =
    case tokenType of
        Namespace ->        0
        Type ->             1
        Class ->            2
        Enum ->             3
        Interface ->        4
        Struct ->           5
        TypeParameter ->    6
        Parameter ->        7
        Variable ->         8
        Property ->         9
        EnumMember ->       10
        Event ->            11
        Function ->         12
        Method ->           13
        Macro ->            14
        Keyword ->          15
        Modifier ->         16
        Comment ->          17
        String_ ->           18
        Number ->           19
        Regexp ->           20
        Operator ->         21



-- TOKEN MODIFIERS


data Modifier
    = Declaration
	| Definition
	| Readonly
	| Static
	| Deprecated
	| Abstract
	| Async
	| Modification
	| Documentation
	| DefaultLibrary


tokenModifiers :: [String]
tokenModifiers =
    [ "declaration"
    , "definition"
    , "readonly"
    , "static"
    , "deprecated"
    , "abstract"
    , "async"
    , "modification"
    , "documentation"
    , "defaultLibrary"
    ]


fromModifierToInt :: Modifier -> Int
fromModifierToInt modifier =
    case modifier of
        Declaration ->      0x001
        Definition ->       0x002
        Readonly ->         0x004
        Static ->           0x008
        Deprecated ->       0x010
        Abstract ->         0x020
        Async ->            0x040
        Modification ->     0x080
        Documentation ->    0x100
        DefaultLibrary ->   0x200