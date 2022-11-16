module CoreFn.Optimise.DeadCode exposing
    ( conservative, aggressive
    , custom
    )

{-|

@docs conservative, aggressive
@docs custom

-}

-- IMPORTS ---------------------------------------------------------------------

import CoreFn.Expr as CoreFn
import CoreFn.Name as CoreFn
import List
import Maybe exposing (Maybe(..))
import Set
import String exposing (String)
import Util.Maybe



-- MANIPULATIONS ---------------------------------------------------------------


{-| -}
conservative : List CoreFn.Name -> CoreFn.Expr ann -> CoreFn.Expr ann
conservative pureFns =
    let
        isSimplePureExpression expr =
            case expr of
                CoreFn.App _ (CoreFn.Var _ name) arg ->
                    List.member name pureFns && isSimplePureExpression arg

                CoreFn.App _ exp arg ->
                    isSimplePureExpression exp && isSimplePureExpression arg

                CoreFn.Lam _ _ _ ->
                    True

                CoreFn.Let _ _ exp bod ->
                    isSimplePureExpression exp && isSimplePureExpression bod

                CoreFn.Lit _ (CoreFn.Arr arr) ->
                    List.all isSimplePureExpression arr

                CoreFn.Lit _ (CoreFn.Con _ args) ->
                    List.all isSimplePureExpression args

                CoreFn.Lit _ (CoreFn.Int _) ->
                    True

                CoreFn.Lit _ (CoreFn.Num _) ->
                    True

                CoreFn.Lit _ (CoreFn.Obj obj) ->
                    List.all (Tuple.second >> isSimplePureExpression) obj

                CoreFn.Lit _ (CoreFn.Str _) ->
                    True

                CoreFn.Pat _ exp cases ->
                    isSimplePureExpression exp
                        && List.all
                            (\( _, grd, bod ) ->
                                isSimplePureExpression bod
                                    && (Maybe.map isSimplePureExpression grd
                                            |> Maybe.withDefault False
                                       )
                            )
                            cases

                CoreFn.Var _ _ ->
                    True
    in
    custom
        (\name -> CoreFn.lowerString name |> Util.Maybe.or (CoreFn.synthString name))
        isSimplePureExpression


{-| -}
aggressive : CoreFn.Expr ann -> CoreFn.Expr ann
aggressive =
    custom
        (\name -> CoreFn.lowerString name |> Util.Maybe.or (CoreFn.synthString name))
        (Basics.always True)


{-| -}
custom : (CoreFn.Name -> Maybe String) -> (CoreFn.Expr ann -> Bool) -> CoreFn.Expr ann -> CoreFn.Expr ann
custom toString isPure expr =
    let
        onApp ann ( fun, funRefs ) ( arg, argRefs ) =
            ( CoreFn.App ann fun arg, Set.union funRefs argRefs )

        onLam ann arg ( bod, refs ) =
            -- To cover semantics where shadowing is acceptable, we need to
            -- *remove* the name from the set of references. This means we can
            -- be sure if the name appears in the set again further up the tree
            -- that it does not include child expressions that reference *this*
            -- binding.
            ( CoreFn.Lam ann arg bod, Set.remove arg refs )

        onLet ann name ( exp, _ ) ( bod, refs ) =
            if Basics.not (Set.member name refs) && isPure exp then
                -- If a let binding is not referenced anywhere in its body, and
                -- the expression it binds is pure, then we can remove it and
                -- simply return the body expression instead.
                ( bod, refs )

            else
                -- To cover semantics where shadowing is acceptable, we need to
                -- *remove* the name from the set of references. This means we
                -- can be sure if the name appears in the set again further up
                -- the tree that it does not include child expressions that
                -- reference *this* binding.
                ( CoreFn.Let ann name exp bod, Set.remove name refs )

        onLit ann lit =
            case lit of
                CoreFn.Arr arr ->
                    ( CoreFn.Lit ann (CoreFn.Arr (List.map Tuple.first arr))
                    , List.foldl (Tuple.second >> Set.union) Set.empty arr
                    )

                CoreFn.Con tag args ->
                    ( CoreFn.Lit ann (CoreFn.Con tag (List.map Tuple.first args))
                    , List.foldl (Tuple.second >> Set.union) Set.empty args
                    )

                CoreFn.Int int ->
                    ( CoreFn.Lit ann (CoreFn.Int int), Set.empty )

                CoreFn.Num num ->
                    ( CoreFn.Lit ann (CoreFn.Num num), Set.empty )

                CoreFn.Obj obj ->
                    ( CoreFn.Lit ann (CoreFn.Obj (List.map (Tuple.mapSecond Tuple.first) obj))
                    , List.foldl (Tuple.second >> Tuple.second >> Set.union) Set.empty obj
                    )

                CoreFn.Str str ->
                    ( CoreFn.Lit ann (CoreFn.Str str), Set.empty )

        onPat ann ( exp, expRefs ) cases =
            List.map onCase cases
                |> List.unzip
                |> (\( caseExps, caseRefs ) ->
                        ( CoreFn.Pat ann exp caseExps
                        , List.foldl Set.union expRefs caseRefs
                        )
                   )

        onCase ( pat, grd, bod ) =
            ( ( pat, Maybe.map Tuple.first grd, Tuple.first bod )
            , Set.union (Tuple.second bod) <| Maybe.withDefault Set.empty <| Maybe.map Tuple.second grd
            )

        onVar ann name =
            case toString name of
                Just var ->
                    ( CoreFn.Var ann name, Set.singleton var )

                Nothing ->
                    ( CoreFn.Var ann name, Set.empty )
    in
    Tuple.first <|
        CoreFn.foldWithAnnotation
            { onApp = onApp
            , onLam = onLam
            , onLet = onLet
            , onLit = onLit
            , onPat = onPat
            , onVar = onVar
            }
            expr
