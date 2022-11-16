module Util.Json exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------

import Json.Decode
import Json.Encode



-- TYPES -----------------------------------------------------------------------
-- CONSTANTS -------------------------------------------------------------------
-- CONSTRUCTORS ----------------------------------------------------------------


encodeTaggedValue : String -> List ( String, Json.Encode.Value ) -> Json.Encode.Value
encodeTaggedValue tag fields =
    Json.Encode.object <| ( "$", Json.Encode.string tag ) :: fields



-- QUERIES ---------------------------------------------------------------------
-- MANIPULATIONS ---------------------------------------------------------------
-- CONVERSIONS -----------------------------------------------------------------


taggedValueDecoder : (String -> Json.Decode.Decoder a) -> Json.Decode.Decoder a
taggedValueDecoder decoder =
    Json.Decode.field "$" Json.Decode.string
        |> Json.Decode.andThen decoder



-- UTILS -----------------------------------------------------------------------
