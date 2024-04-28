module Lsp.Id (Id, fromInt, fromString, decoder, encode) where

import qualified Json.Decode
import qualified Json.Encode


-- ID


data Id = StringId String | IntId Int


fromInt :: Int -> Id
fromInt int = IntId int


fromString :: String -> Id
fromString str = StringId str



-- JSON


decoder :: Json.Decode.Decoder Id
decoder =
    Json.Decode.oneOf
        [ Json.Decode.map StringId Json.Decode.string
        , Json.Decode.map IntId Json.Decode.int
        ]


encode :: Id -> Json.Encode.Value
encode id =
    case id of
        IntId int -> Json.Encode.int int 
        StringId string -> Json.Encode.string string