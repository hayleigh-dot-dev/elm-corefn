module CoreFn.Optimise.ConstProp exposing
    ( literals
    , custom
    )

{-|

@docs literals
@docs custom

-}

-- IMPORTS ---------------------------------------------------------------------

import CoreFn.Expr as CoreFn
import CoreFn.Name as CoreFn
import Dict
import Set



-- MANIPULATIONS ---------------------------------------------------------------


{-| -}
literals : CoreFn.Expr ann -> CoreFn.Expr ann
literals =
    let
        isSimpleLiteral expr =
            case expr of
                CoreFn.Lit _ (CoreFn.Con _ []) ->
                    True

                CoreFn.Lit _ (CoreFn.Num _) ->
                    True

                CoreFn.Lit _ (CoreFn.Str _) ->
                    True

                _ ->
                    False
    in
    custom isSimpleLiteral


{-| -}
custom : (CoreFn.Expr ann -> Bool) -> CoreFn.Expr ann -> CoreFn.Expr ann
custom canPropagate expr =
    let
        onApp ann fun arg bindings =
            CoreFn.App ann (fun bindings) (arg bindings)

        onLam ann arg bod bindings =
            CoreFn.Lam ann arg <| bod <| Dict.remove arg bindings

        onLet ann name exp bod bindings =
            (\exp_ -> CoreFn.Let ann name exp_ (bod (Dict.insert name exp_ bindings))) <| exp bindings

        onLit ann lit bindings =
            case lit of
                CoreFn.Arr arr ->
                    CoreFn.Lit ann <| CoreFn.Arr <| List.map ((|>) bindings) arr

                CoreFn.Con tag args ->
                    CoreFn.Lit ann <| CoreFn.Con tag <| List.map ((|>) bindings) args

                CoreFn.Int int ->
                    CoreFn.Lit ann <| CoreFn.Int int

                CoreFn.Num num ->
                    CoreFn.Lit ann <| CoreFn.Num num

                CoreFn.Obj obj ->
                    CoreFn.Lit ann <| CoreFn.Obj <| List.map (Tuple.mapSecond ((|>) bindings)) obj

                CoreFn.Str str ->
                    CoreFn.Lit ann <| CoreFn.Str str

        onPat ann exp cases bindings =
            CoreFn.Pat ann (exp bindings) <|
                List.map
                    (\( pat, grd, bod ) ->
                        let
                            bindings_ =
                                CoreFn.patternBindings pat
                                    |> Set.foldl Dict.remove bindings
                        in
                        ( pat, Maybe.map ((|>) bindings_) grd, bod bindings_ )
                    )
                    cases

        onVar ann name bindings =
            case Dict.get (CoreFn.toString "." name) bindings of
                Just exp ->
                    if canPropagate exp then
                        exp

                    else
                        CoreFn.Var ann name

                Nothing ->
                    CoreFn.Var ann name
    in
    CoreFn.foldWithAnnotation
        { onApp = onApp
        , onLam = onLam
        , onLet = onLet
        , onLit = onLit
        , onPat = onPat
        , onVar = onVar
        }
        expr
        Dict.empty
