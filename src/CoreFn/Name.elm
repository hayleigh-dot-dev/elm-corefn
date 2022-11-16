module CoreFn.Name exposing
    ( Name(..)
    , scoped
    , toString, lowerString, upperString, synthString
    , encode, decoder
    )

{-|


## Types

@docs Name


## Constructors

@docs scoped


## Conversions

@docs toString, lowerString, upperString, synthString


## JSON

@docs encode, decoder

-}

-- IMPORTS ---------------------------------------------------------------------

import Basics exposing (Int)
import Json.Decode
import Json.Encode
import Maybe exposing (Maybe(..))
import String exposing (String)
import Util.Json



-- TYPES -----------------------------------------------------------------------


{-| -}
type Name
    = Lower String
    | Upper String
    | Synth Int
    | Qualified String Name



-- CONSTRUCTORS ----------------------------------------------------------------


{-| -}
scoped : List String -> Name -> Name
scoped quals name =
    case quals of
        [] ->
            name

        qual :: rest ->
            Qualified qual (scoped rest name)



-- QUERIES ---------------------------------------------------------------------
-- MANIPULATIONS ---------------------------------------------------------------
-- CONVERSIONS -----------------------------------------------------------------


{-| -}
toString : String -> Name -> String
toString sep name =
    case name of
        Lower str ->
            str

        Upper str ->
            str

        Synth int ->
            String.fromInt int

        Qualified qualifier name_ ->
            qualifier ++ sep ++ toString sep name_


{-| -}
lowerString : Name -> Maybe String
lowerString name =
    case name of
        Lower str ->
            Just str

        _ ->
            Nothing


{-| -}
upperString : Name -> Maybe String
upperString name =
    case name of
        Upper str ->
            Just str

        _ ->
            Nothing


{-| -}
synthString : Name -> Maybe String
synthString name =
    case name of
        Synth int ->
            Just <| String.fromInt int

        _ ->
            Nothing



-- JSON ------------------------------------------------------------------------


{-| -}
encode : Name -> Json.Encode.Value
encode name =
    case name of
        Lower var ->
            Util.Json.encodeTaggedValue "Lower"
                [ ( "var", Json.Encode.string var )
                ]

        Upper var ->
            Util.Json.encodeTaggedValue "Upper"
                [ ( "var", Json.Encode.string var )
                ]

        Synth id ->
            Util.Json.encodeTaggedValue "Synth"
                [ ( "id", Json.Encode.int id )
                ]

        Qualified qualifier var ->
            Util.Json.encodeTaggedValue "Qualified"
                [ ( "qualifier", Json.Encode.string qualifier )
                , ( "var", encode var )
                ]


{-| -}
decoder : Json.Decode.Decoder Name
decoder =
    Util.Json.taggedValueDecoder <| \tag ->
    case tag of
        "Lower" ->
            Json.Decode.map Lower
                (Json.Decode.field "var" Json.Decode.string)

        "Upper" ->
            Json.Decode.map Upper
                (Json.Decode.field "var" Json.Decode.string)

        "Synth" ->
            Json.Decode.map Synth
                (Json.Decode.field "id" Json.Decode.int)

        "Qualified" ->
            Json.Decode.map2 Qualified
                (Json.Decode.field "qualifier" Json.Decode.string)
                (Json.Decode.field "var" <| Json.Decode.lazy (\_ -> decoder))

        _ ->
            Json.Decode.fail <| "[CoreFn.Name.decoder] Unknown tag: " ++ tag
