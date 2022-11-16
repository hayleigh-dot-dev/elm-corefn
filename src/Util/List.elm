module Util.List exposing (..)

{-| -}

-- IMPORTS ---------------------------------------------------------------------
-- TYPES -----------------------------------------------------------------------
-- CONSTANTS -------------------------------------------------------------------
-- CONSTRUCTORS ----------------------------------------------------------------
-- QUERIES ---------------------------------------------------------------------


find : (a -> Bool) -> List a -> Maybe a
find predicate xs =
    case xs of
        x :: rest ->
            if predicate x then
                Just x

            else
                find predicate rest

        [] ->
            Nothing


findMap : (a -> Maybe b) -> List a -> Maybe b
findMap f xs =
    case xs of
        x :: rest ->
            case f x of
                Just y ->
                    Just y

                Nothing ->
                    findMap f rest

        [] ->
            Nothing



-- MANIPULATIONS ---------------------------------------------------------------
-- CONVERSIONS -----------------------------------------------------------------
-- UTILS -----------------------------------------------------------------------
