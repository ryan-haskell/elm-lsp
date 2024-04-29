module Parser.Core (
    Parser, run,
    succeed, fail,
    keyword, spaces,
    chompWhile, getChompedString,
    oneOf, map,
    Located(..), Range, located,
    isWhitespace,
    (|=), (|.)
) where

import Prelude hiding (fail, map)
import qualified Data.List as List
import Data.Function ((&))



-- DATA STRUCTURES


data Parser value
  = Parser (State -> Step value)


data State
    = State Source Int Location
    deriving (Show) -- FOR DEBUGGING ONLY


data Step value
    = Good State value
    | Bad State Problem
    deriving (Show) -- FOR DEBUGGING ONLY


data Problem
    = ExpectedKeyword String
    | ExpectedParsersInOneOf
    | Custom String
    deriving (Show) -- FOR DEBUGGING ONLY


type Source = String



-- DEFINING PARSERS


{-| Immediately succeeds the parser with the provided value.
-}
succeed :: value -> Parser value
succeed value =
    Parser (\state -> Good state value)


{-| Immediately fails the parser with the provided reason
-}
fail :: String -> Parser value
fail reason =
    Parser (\state -> Bad state (Custom reason))


{-| Expect an exact match of the provided keyword
-}
keyword :: String -> Parser ()
keyword str =
    let
        parser :: State -> Step ()
        parser (State source offset ( line, start, len )) =
            if List.isPrefixOf str source then
                Good (State (drop (length str) source) 0 ( line, start + length str, len + length str )) ()

            else
                Bad (State source offset ( line, start, len )) (ExpectedKeyword str)
    in
    Parser (resetChompOffset parser)


{-| Gobbles up any spaces, new lines, carriage returns, etc
-}
spaces :: Parser ()
spaces =
    chompWhile isWhitespace


isWhitespace :: Char -> Bool
isWhitespace char =
    char == ' ' || char == '\n' || char == '\r'


{-| Moves through a parser as long as the condition is met. 

__Note:__ This always succeeds, even if there weren't any
characters that matched the condition.
-}
chompWhile :: (Char -> Bool) -> Parser ()
chompWhile predicate =
    let
        parser :: State -> Step ()
        parser (State source offset location) =
            case drop offset source of
                [] ->
                    Good (State source offset location) ()

                (char:_) ->
                    if predicate char then
                        parser (State source (offset + 1) (nextLocation char location))

                    else
                        Good (State source offset location) ()
    in
    Parser (resetChompOffset parser)


nextLocation :: Char -> Location -> Location
nextLocation char ( line, start, offset ) =
    if char == '\n' || char == '\r' then
        ( line + 1, 1, offset + 1 )

    else
        ( line, start + 1, offset + 1 )


{-| Get the string that has been chomped so far
-}
getChompedString :: Parser x -> Parser String
getChompedString (Parser fn) =
    let
        parser :: State -> Step String
        parser state =
            case fn state of
                Good (State source offset location) _ ->
                    Good
                        (State (drop offset source) 0 location)
                        (take offset source)

                Bad newState problem ->
                    Bad newState problem
    in
    Parser parser


oneOf :: [Parser value] -> Parser value
oneOf parsers =
    case parsers of
        [] ->
            Parser (\state -> Bad state ExpectedParsersInOneOf)

        (first:rest) ->
            Parser (\state -> oneOfHelp first rest state)


map :: (a -> b) -> Parser a -> Parser b
map fn (Parser parserA) =
    let
        -- 👇 Whoa, Haskell is crazy af, this makes a DIFFERENT "b" variable??
        -- parser :: State -> Step b 
        parser state =
            case parserA state of
                Good newState a ->
                    Good newState (fn a)
                
                Bad newState problem ->
                    Bad newState problem
    in
    Parser parser



-- COMBINING PARSERS


infixl 1 |=
infixl 1 |.


{-| Runs the parser and passes the result along so it can be applied to the
data structure we're trying to build.
-}
(|=) :: Parser (field -> value) -> Parser field -> Parser value
a |= b =
    map2 ($) a b


{-| Runs the parser to make progress, but the result won't be used in the
data structure we're trying to build.
-}
(|.) :: Parser value -> Parser x -> Parser value
a |. b =
    map2 (\keep_ ignore -> keep_) a b



-- RUNNING PARSERS

{-| Runs a parser, returning either a problem or the parsed value.
-}
run :: Parser value -> String -> Either Problem value
run parser source =
    case toStep parser source of
        Good _ value ->
            Right value

        Bad _ problem ->
            Left problem



-- INTERNAL STUFF


{-| Runs the parser -}
toStep :: Parser value -> String -> Step value
toStep (Parser parseFn) source =
    parseFn (toInitialState source)


{-| The initial value of our state when our parser starts running/ -}
toInitialState :: String -> State
toInitialState source =
    State source 0 ( 1, 1, 0 )


{-| Both `keep` and `ignore` behave in similar ways, so 
having this function will make my life easier! -}
map2 :: (a -> b -> value) -> Parser a -> Parser b -> Parser value
map2 fn (Parser parserA) (Parser parserB) =
    Parser 
        (\s0 ->
            case parserA s0 of
                Bad s1 problem ->
                    Bad s1 problem

                Good s1 a -> 
                    case parserB s1 of
                        Bad s2 problem ->
                            Bad s2 problem

                        Good s2 b ->
                            Good s2 (fn a b)
        )


{-| TODO: Is this the "right" way to prevent `getChompedString` from getting
previously chomped things? -}
resetChompOffset :: (State -> Step a) -> State -> Step a
resetChompOffset parser (State source offset location) =
    parser (State (drop offset source) 0 location)


oneOfHelp :: Parser value -> [Parser value] -> State -> Step value
oneOfHelp (Parser parse) rest state =
    case parse state of
        Good newState value ->
            Good newState value

        Bad newState problem ->
            case rest of
                [] ->
                    Bad newState problem

                (newFirst:newRest) ->
                    oneOfHelp newFirst newRest state

-- LOCATIONS


data Located value = Located Range value
    deriving (Show)


located :: Parser value -> Parser (Located value)
located (Parser parse) =
    Parser
        (\state ->
            case parse state of
                Good newState value ->
                    Good newState (Located (toRange state newState) value)
                
                Bad newState problem ->
                    Bad newState problem
        )


-- TODO: I think this is busted
toRange :: State -> State -> Range
toRange (State _ _ ( oldLine, oldStart, oldLength )) (State _ _ ( newLine, newStart, newLength )) =
    ( oldLine
    , oldStart
    , newLength - oldLength
    )


type Range = ( Line, Start, Length )

type Location = ( Line, Start, Length )

type Line = Int
type Start = Int
type Length = Int