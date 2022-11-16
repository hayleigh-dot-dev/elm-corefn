module Util.Maybe exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------
-- TYPES -----------------------------------------------------------------------
-- CONSTANTS -------------------------------------------------------------------
-- CONSTRUCTORS ----------------------------------------------------------------


or : Maybe a -> Maybe a -> Maybe a
or b a =
    case a of
        Just _ ->
            a

        Nothing ->
            b



-- QUERIES ---------------------------------------------------------------------
-- MANIPULATIONS ---------------------------------------------------------------
-- CONVERSIONS -----------------------------------------------------------------
-- UTILS -----------------------------------------------------------------------