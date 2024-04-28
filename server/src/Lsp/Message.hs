module Lsp.Message (Message(..), decoder) where

import Data.Function ((&))
import qualified Json.Decode
import qualified Json.Encode
import qualified Lsp.NotificationMessage
import qualified Lsp.RequestMessage


data Message
    = Request Lsp.RequestMessage.RequestMessage
    | Notification Lsp.NotificationMessage.NotificationMessage


decoder :: Json.Decode.Decoder Message
decoder =
    Json.Decode.oneOf
        [ Json.Decode.map Request Lsp.RequestMessage.decoder
        , Json.Decode.map Notification Lsp.NotificationMessage.decoder
        ]
