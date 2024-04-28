module Json.Decode (
    Decoder, decode
    , Problem(..), fromProblemToString
    , succeed, fail
    , string, int
    , object, with, field, withField
    , Value, value
    , map, map2, andThen, oneOf
) where

import Prelude hiding (map, fail)
import qualified Prelude
import qualified Json.Value
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Value
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Scientific
import qualified Data.Text
import qualified Data.List
import qualified Data.Vector



-- DECODER TYPE


data Decoder value =
    Decoder (Value.Value -> Either Problem value)


data Problem
    = InvalidJson
    | ExpectedType String
    | ExpectedField String
    | ProblemWithField String Problem
    | ProblemAtIndex Int Problem
    | ProblemWithOneOf [Problem]
    | Failure String


fromProblemToString :: Problem -> String
fromProblemToString problem =
    fromStringPathToString 0 (fromProblemToStringWithPath 0 ( problem, [] ))


decode :: String -> Decoder value -> Either Problem value
decode jsonString (Decoder toResult) =
    case fromStringToValue jsonString of
        Nothing -> Left InvalidJson
        Just value -> toResult value



-- SUCCEED


succeed :: value -> Decoder value
succeed value =
    Decoder (\_ -> Right value)


fail :: String -> Decoder value
fail reason =
    Decoder (\_ -> Left (Failure reason))



-- PRIMITIVES


string :: Decoder String
string =
    Decoder (\value ->
        case value of
            Value.String text -> Right (Data.Text.unpack text)
            _ -> Left (ExpectedType "string")
    )


int :: Decoder Int
int =
    Decoder (\value ->
        case value of
            Value.Number scientific ->
                case Data.Scientific.toBoundedInteger scientific of
                    Nothing -> Left (ExpectedType "int")
                    Just int -> Right int
            _ -> Left (ExpectedType "int")
    )



-- OPTIONAL VALUES


maybe :: Decoder value -> Decoder (Maybe value)
maybe decoder =
    oneOf
        [ map Just decoder
        , succeed Nothing
        ]



-- LISTS


list :: Decoder value -> Decoder [value]
list (Decoder toEither) =
    Decoder (\value ->
        case value of
            Value.Array vector ->
                case Data.Vector.toList vector of
                    [] -> Right []
                    listOfValues -> flattenEitherList (Prelude.map toEither listOfValues)

            _ ->
                Left (ExpectedType "array")
    )


flattenEitherList :: [Either Problem value] -> Either Problem [value]
flattenEitherList listOfEithers =
    case flattenEitherListHelp 0 [] listOfEithers of
        Left (index, problem) ->
            Left (ProblemAtIndex index problem)

        Right values ->
            Right (reverse values)


flattenEitherListHelp :: Int -> [value] -> [Either Problem value] -> Either (Int, Problem) [value]
flattenEitherListHelp index listSoFar listOfEithers =
    case listOfEithers of
        [] ->
            Right []

        (Left problem : _) ->
            Left ( index, problem )

        (Right value : remainingEithers) ->
            flattenEitherListHelp (index + 1) (value : listSoFar) remainingEithers 



-- OBJECTS


object :: value -> Decoder value
object x =
    succeed x


with :: Decoder a -> Decoder (a -> b) -> Decoder b
with fieldDecoder fnDecoder =
    map2 (\field fn -> fn field) fieldDecoder fnDecoder


field :: String -> Decoder value -> Decoder value
field key (Decoder innerDecoder) =
    Decoder (\value ->
        case value of
            Value.Object keyMap ->
                case KeyMap.lookup (Key.fromString key) keyMap of
                    Nothing ->
                        Left (ExpectedField key)

                    Just innerValue ->
                        case innerDecoder innerValue of
                            Right value_ ->
                                Right value_

                            Left innerProblem ->
                                Left (ProblemWithField key innerProblem)

            _ ->
                Left (ExpectedType "object")

    )


{-| Helper function for working with required fields -}
withField :: String -> Decoder field -> Decoder (field -> value) -> Decoder value
withField fieldName fieldDecoder fnDecoder =
    with (field fieldName fieldDecoder) fnDecoder



-- TRANSFORMING DECODERS


map :: (a -> b) -> Decoder a -> Decoder b
map fn (Decoder toEitherA) =
    Decoder (\value ->
        mapEither fn (toEitherA value)
    )


map2 :: (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 fn (Decoder toEitherA) (Decoder toEitherB) =
    Decoder (\value ->
        case (toEitherA value, toEitherB value) of
            (Left problem, _) -> Left problem
            (_, Left problem) -> Left problem
            (Right a, Right b) -> Right (fn a b)
    )


andThen :: (a -> Decoder b) -> Decoder a -> Decoder b
andThen toDecoder (Decoder toEitherA) =
    Decoder (\value ->
        case toEitherA value of
            Left problem ->
                Left problem

            Right a ->
                let 
                    (Decoder toEitherB) = toDecoder a
                in
                toEitherB value

    )


oneOf :: [Decoder value] -> Decoder value
oneOf list = 
    Decoder (\value ->
        let
            tryDecoder :: Decoder value -> [Decoder value] -> [Problem] -> Either [Problem] value
            tryDecoder (Decoder toEither) otherDecoders problemsSoFar =
                case toEither value of
                    Right value_ ->
                        Right value_

                    Left problem ->
                        case otherDecoders of
                            [] ->
                                Left (problem : problemsSoFar)

                            (nextDecoder:newOtherDecoders) ->
                                tryDecoder nextDecoder newOtherDecoders (problem : problemsSoFar)
        in
        case list of
            [] -> Left (ProblemWithOneOf [])

            (decoder : otherDecoders) ->
                case tryDecoder decoder otherDecoders [] of
                    Left problems -> Left (ProblemWithOneOf (reverse problems))
                    Right value -> Right value
    )



-- DELAYING DECODING


type Value = Json.Value.Value


value :: Decoder Value
value =
    Decoder (\val -> Right (Json.Value.Value val))



-- FORMATTING JSON ERRORS


fromStringPathToString :: Int -> ( String, Path ) -> String
fromStringPathToString indentation ( problem, segments ) =
    let
        prefix :: String
        prefix =
            "-- PROBLEM AT \""

        pathAsString :: String
        pathAsString =
            case segments of
                [] -> "TOP-LEVEL"
                _ -> Data.List.intercalate "" (Prelude.map fromSegmentToString segments)

        headerDashCount :: Int
        headerDashCount =
            max 3 (80 - 2 - indentation - length prefix - length pathAsString)

    in
    prefix <> pathAsString <> "\" " <> repeatString headerDashCount "-" <> "\n\n" <>
    problem <> "\n\n" <>
    repeatString (80 - indentation) "-"


type Path = [Segment]


data Segment
    = Field String
    | Index Int


fromSegmentToString :: Segment -> String
fromSegmentToString segment =
    case segment of
        Field name -> "." <> name
        Index index -> "[" <> show index <> "]"


fromProblemToStringWithPath :: Int -> ( Problem, Path ) -> ( String, Path )
fromProblemToStringWithPath indentation ( problem, path ) =
    case problem of
        InvalidJson ->
            ("JSON failed to parse.", path)

        ExpectedType name ->
            ("Expected type: " <> name, path)

        ExpectedField name ->
            ("Expected field: \"" <> name <> "\"", path)

        ProblemWithField fieldName innerProblem ->
            fromProblemToStringWithPath indentation
                ( innerProblem
                , path <> [Field fieldName]
                )

        ProblemWithOneOf [] ->
            ( "Expected a non-empty list with \"oneOf\"", path )

        ProblemWithOneOf innerProblems ->
            ( "Failed in one of these ways:\n\n"
                <> (Data.List.intercalate "\n"
                        (Prelude.map
                            (\innerProblem -> indent 4 (fromStringPathToString (indentation + 4) (fromProblemToStringWithPath (indentation + 4) (innerProblem, path))))
                            innerProblems
                        )
                    )
            , path
            )

        Failure reason ->
            ( reason, path )



-- INTERNALS

fromStringToValue :: String -> Maybe Aeson.Value
fromStringToValue jsonString =
  case Aeson.decode (BL.fromStrict (B8.pack jsonString)) of
    Nothing -> Nothing
    Just jsonValue -> Just jsonValue


mapEither :: (a -> b) -> Either x a -> Either x b
mapEither fn either =
    case either of
        Left problem -> Left problem
        Right value -> Right (fn value)

indent :: Int -> String -> String
indent n = unlines . Prelude.map ("  | " ++) . lines

repeatString :: Int -> String -> String
repeatString n str = concatMap (const str) [1..n]