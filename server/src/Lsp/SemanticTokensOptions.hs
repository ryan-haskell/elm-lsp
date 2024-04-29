module Lsp.SemanticTokensOptions (
    SemanticTokensOptions(..)
    , create, encode
) where

import qualified Json.Encode
import qualified Lsp.Capabilities.SemanticTokens

data SemanticTokensOptions = SemanticTokensOptions
    { legend :: SemanticTokensLegend 
    , range :: Bool
    , full :: Bool
    }


data SemanticTokensLegend = SemanticTokensLegend
    { tokenTypes :: [String]
    , tokenModifiers :: [String]
    }


create :: SemanticTokensOptions
create =
    SemanticTokensOptions
        (SemanticTokensLegend
            Lsp.Capabilities.SemanticTokens.tokenTypes
            Lsp.Capabilities.SemanticTokens.tokenModifiers
        )
        False
        True



-- JSON


encode :: SemanticTokensOptions -> Json.Encode.Value
encode options =
    Json.Encode.object
        [ ( "legend", encodeSemanticTokensLegend (legend options) )
        , ( "range", Json.Encode.bool (range options) )
        , ( "full", Json.Encode.bool (full options) )
        ]


encodeSemanticTokensLegend :: SemanticTokensLegend -> Json.Encode.Value
encodeSemanticTokensLegend legend =
    Json.Encode.object
        [ ( "tokenTypes", Json.Encode.list Json.Encode.string (tokenTypes legend) )
        , ( "tokenModifiers", Json.Encode.list Json.Encode.string (tokenModifiers legend) )
        ]
