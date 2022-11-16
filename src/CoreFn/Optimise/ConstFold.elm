module CoreFn.Optimise.ConstFold exposing
    ( common, custom
    , unop, binop
    , intUnop, numUnop, strUnop
    , intBinop, numBinop, strBinop
    , f, f2, f3, f4, f5, f6, f7, f8
    )

{-|

@docs common, custom


## Operators

@docs unop, binop


### Typed Operators

@docs intUnop, numUnop, strUnop
@docs intBinop, numBinop, strBinop


## Functions

The following collection of functions reduce some of the boilerplate when
optimising for n-arity functions. Because the `CoreFn` expression models
function application as repeaded application of single-argument functions, there
ends up being quite a bit of nesting if you want to pull out 3, 4, ..8 arguments!

@docs f, f2, f3, f4, f5, f6, f7, f8

-}

-- IMPORTS ---------------------------------------------------------------------

import Basics
import CoreFn.Expr as CoreFn
import CoreFn.Name as CoreFn
import List
import Maybe exposing (Maybe(..))



-- MANIPULATIONS ---------------------------------------------------------------


{-| A convenient helper to eliminate common maths expressions and other operators.
Like all the optimisations in this module this will only eliminate expressions
that only contain literals, but when combined with the constant propagation
helpers defined in [`ConstProp`](./CoreFn-Optimise-ConstProp) you can eliminate
constant variables too.

This optimisation is applied bottom-up, so it will eliminate compound expressions
like `1 + 2 * 3` (assuming you provide names for your `add` and `mul` operators).

-}
common :
    { add : Maybe CoreFn.Name
    , sub : Maybe CoreFn.Name
    , mul : Maybe CoreFn.Name
    , div : Maybe CoreFn.Name

    --
    , neg : Maybe CoreFn.Name
    }
    -> (ann -> ann -> ann)
    -> CoreFn.Expr ann
    -> CoreFn.Expr ann
common ops mergeAnn =
    custom <|
        List.filterMap Basics.identity
            [ Maybe.map (\name -> numBinop name mergeAnn (+)) ops.add
            , Maybe.map (\name -> intBinop name mergeAnn (+)) ops.add
            , Maybe.map (\name -> numBinop name mergeAnn (-)) ops.sub
            , Maybe.map (\name -> intBinop name mergeAnn (-)) ops.sub
            , Maybe.map (\name -> numBinop name mergeAnn (*)) ops.mul
            , Maybe.map (\name -> intBinop name mergeAnn (*)) ops.mul
            , Maybe.map (\name -> numBinop name mergeAnn (/)) ops.div
            , Maybe.map (\name -> intBinop name mergeAnn (//)) ops.div

            --
            , Maybe.map (\name -> numUnop name Basics.negate) ops.neg
            , Maybe.map (\name -> intUnop name Basics.negate) ops.neg
            ]


{-| Allows you to supply a list of custom eliminations that may or may not
succeed. Best used in combination with the helpers defined elsewhere in this
module like [`unop`](#unop), [`binop`](#binop), [`fn`](#fn), and so on.
-}
custom :
    List (CoreFn.Expr ann -> Maybe (CoreFn.Expr ann))
    -> CoreFn.Expr ann
    -> CoreFn.Expr ann
custom fns =
    CoreFn.transformMany (List.map (\fun expr -> Maybe.withDefault expr <| fun expr) fns)



-- UNARY OPERATORS -------------------------------------------------------------


{-| Eliminate a unary operator applied to some literal. We could implement an
elimination for simple negation like so:

    import CoreFn.Expr as Expr exposing (Expr)
    import CoreFn.Name as Name
    import CoreFn.Optimise.ConstFold as ConstFold

    negate : Expr -> Maybe Expr
    negate =
        ConstFold.unop
            (Name.Lower "negate")
            (\lit ->
                case lit of
                    Expr.Int int ->
                        Just <| Expr.Int <| Basics.negate int

                    Expr.Num num ->
                        Just <| Expr.Num <| Basics.negate num

                    _ ->
                        Nothing
            )

-}
unop :
    CoreFn.Name
    -> (CoreFn.Lit (CoreFn.Expr ann) -> Maybe (CoreFn.Lit (CoreFn.Expr ann)))
    -> CoreFn.Expr ann
    -> Maybe (CoreFn.Expr ann)
unop name mergeLit =
    f name <| \a ->
    case a of
        CoreFn.Lit ann lit ->
            Maybe.map (CoreFn.Lit ann) <| mergeLit lit

        _ ->
            Nothing


{-| -}
intUnop :
    CoreFn.Name
    -> (Int -> Int)
    -> CoreFn.Expr ann
    -> Maybe (CoreFn.Expr ann)
intUnop name fn =
    unop name <| \lit ->
    case lit of
        CoreFn.Int int ->
            Just <| CoreFn.Int <| fn int

        _ ->
            Nothing


{-| -}
numUnop :
    CoreFn.Name
    -> (Float -> Float)
    -> CoreFn.Expr ann
    -> Maybe (CoreFn.Expr ann)
numUnop name fn =
    unop name <| \lit ->
    case lit of
        CoreFn.Num num ->
            Just <| CoreFn.Num <| fn num

        _ ->
            Nothing


{-| -}
strUnop :
    CoreFn.Name
    -> (String -> String)
    -> CoreFn.Expr ann
    -> Maybe (CoreFn.Expr ann)
strUnop name fn =
    unop name <| \lit ->
    case lit of
        CoreFn.Str str ->
            Just <| CoreFn.Str <| fn str

        _ ->
            Nothing



-- BINARY OPERATORS ------------------------------------------------------------


{-| Just like [`unop`](#unop), we can eliminate a binary operator applied to two
literal values with this function. You can probably guess how to implement
something like addition (there are [even](#numBinop) [helpers](#intBinop) to make
that easier) so let's look at a different example.

We could eliminate object/record access like so:

    import CoreFn.Expr as Expr exposing (Expr)
    import CoreFn.Name as Name
    import CoreFn.Optimise.ConstFold as ConstFold

    access : Expr -> Maybe Expr
    access =
        ConstFold.binop
            (Name.Lower "access")
            (\lhs rhs ->
                case ( lhs, rhs ) of
                    ( Expr.Str key, Expr.Obj obj ) ->
                        List.filter (Tuple.first >> (==) key) obj
                            |> List.head
                            |> Maybe.map Tuple.second

                    _ ->
                        Nothing
            )

Such that if we're given...

    { foo = 1 }.foo

...we can eliminate the whole thing to...

    1

-}
binop :
    CoreFn.Name
    -> (ann -> ann -> ann)
    -> (CoreFn.Lit (CoreFn.Expr ann) -> CoreFn.Lit (CoreFn.Expr ann) -> Maybe (CoreFn.Lit (CoreFn.Expr ann)))
    -> CoreFn.Expr ann
    -> Maybe (CoreFn.Expr ann)
binop name mergeAnn mergeLit =
    f2 name <| \a b ->
    case ( a, b ) of
        ( CoreFn.Lit lhsAnn lhs, CoreFn.Lit rhsAnn rhs ) ->
            mergeLit lhs rhs
                |> Maybe.map (CoreFn.Lit (mergeAnn lhsAnn rhsAnn))

        _ ->
            Nothing


{-| -}
numBinop :
    CoreFn.Name
    -> (ann -> ann -> ann)
    -> (Float -> Float -> Float)
    -> CoreFn.Expr ann
    -> Maybe (CoreFn.Expr ann)
numBinop name mergeAnn mergeNum =
    binop name mergeAnn <| \lhs rhs ->
    case ( lhs, rhs ) of
        ( CoreFn.Num x, CoreFn.Num y ) ->
            Just <| CoreFn.Num <| mergeNum x y

        _ ->
            Nothing


{-| -}
intBinop :
    CoreFn.Name
    -> (ann -> ann -> ann)
    -> (Int -> Int -> Int)
    -> CoreFn.Expr ann
    -> Maybe (CoreFn.Expr ann)
intBinop name mergeAnn mergeNum =
    binop name mergeAnn <| \lhs rhs ->
    case ( lhs, rhs ) of
        ( CoreFn.Int x, CoreFn.Int y ) ->
            Just <| CoreFn.Int <| mergeNum x y

        _ ->
            Nothing


{-| -}
strBinop :
    CoreFn.Name
    -> (ann -> ann -> ann)
    -> (String -> String -> String)
    -> CoreFn.Expr ann
    -> Maybe (CoreFn.Expr ann)
strBinop name mergeAnn mergeNum =
    binop name mergeAnn <| \lhs rhs ->
    case ( lhs, rhs ) of
        ( CoreFn.Str x, CoreFn.Str y ) ->
            Just <| CoreFn.Str <| mergeNum x y

        _ ->
            Nothing



-- FUNCTIONS -------------------------------------------------------------------


{-| -}
f :
    CoreFn.Name
    -> (CoreFn.Expr ann -> Maybe (CoreFn.Expr ann))
    -> CoreFn.Expr ann
    -> Maybe (CoreFn.Expr ann)
f name merge expr =
    case expr of
        CoreFn.App _ (CoreFn.Var _ name_) a ->
            if name == name_ then
                merge a

            else
                Nothing

        _ ->
            Nothing


{-| -}
f2 : CoreFn.Name -> (CoreFn.Expr ann -> CoreFn.Expr ann -> Maybe (CoreFn.Expr ann)) -> CoreFn.Expr ann -> Maybe (CoreFn.Expr ann)
f2 name merge expr =
    case expr of
        CoreFn.App _ (CoreFn.App _ (CoreFn.Var _ name_) a) b ->
            if name == name_ then
                merge a b

            else
                Nothing

        _ ->
            Nothing


{-| -}
f3 :
    CoreFn.Name
    -> (CoreFn.Expr ann -> CoreFn.Expr ann -> CoreFn.Expr ann -> Maybe (CoreFn.Expr ann))
    -> CoreFn.Expr ann
    -> Maybe (CoreFn.Expr ann)
f3 name merge expr =
    case expr of
        CoreFn.App _ (CoreFn.App _ (CoreFn.App _ (CoreFn.Var _ name_) a) b) c ->
            if name == name_ then
                merge a b c

            else
                Nothing

        _ ->
            Nothing


{-| -}
f4 :
    CoreFn.Name
    -> (CoreFn.Expr ann -> CoreFn.Expr ann -> CoreFn.Expr ann -> CoreFn.Expr ann -> Maybe (CoreFn.Expr ann))
    -> CoreFn.Expr ann
    -> Maybe (CoreFn.Expr ann)
f4 name merge expr =
    case expr of
        CoreFn.App _ (CoreFn.App _ (CoreFn.App _ (CoreFn.App _ (CoreFn.Var _ name_) a) b) c) d ->
            if name == name_ then
                merge a b c d

            else
                Nothing

        _ ->
            Nothing


{-| -}
f5 :
    CoreFn.Name
    -> (CoreFn.Expr ann -> CoreFn.Expr ann -> CoreFn.Expr ann -> CoreFn.Expr ann -> CoreFn.Expr ann -> Maybe (CoreFn.Expr ann))
    -> CoreFn.Expr ann
    -> Maybe (CoreFn.Expr ann)
f5 name merge expr =
    case expr of
        CoreFn.App _ (CoreFn.App _ (CoreFn.App _ (CoreFn.App _ (CoreFn.App _ (CoreFn.Var _ name_) a) b) c) d) e ->
            if name == name_ then
                merge a b c d e

            else
                Nothing

        _ ->
            Nothing


{-| -}
f6 :
    CoreFn.Name
    -> (CoreFn.Expr ann -> CoreFn.Expr ann -> CoreFn.Expr ann -> CoreFn.Expr ann -> CoreFn.Expr ann -> CoreFn.Expr ann -> Maybe (CoreFn.Expr ann))
    -> CoreFn.Expr ann
    -> Maybe (CoreFn.Expr ann)
f6 name merge expr =
    case expr of
        CoreFn.App _ (CoreFn.App _ (CoreFn.App _ (CoreFn.App _ (CoreFn.App _ (CoreFn.App _ (CoreFn.Var _ name_) a) b) c) d) e) f_ ->
            if name == name_ then
                merge a b c d e f_

            else
                Nothing

        _ ->
            Nothing


{-| -}
f7 :
    CoreFn.Name
    -> (CoreFn.Expr ann -> CoreFn.Expr ann -> CoreFn.Expr ann -> CoreFn.Expr ann -> CoreFn.Expr ann -> CoreFn.Expr ann -> CoreFn.Expr ann -> Maybe (CoreFn.Expr ann))
    -> CoreFn.Expr ann
    -> Maybe (CoreFn.Expr ann)
f7 name merge expr =
    case expr of
        CoreFn.App _ (CoreFn.App _ (CoreFn.App _ (CoreFn.App _ (CoreFn.App _ (CoreFn.App _ (CoreFn.App _ (CoreFn.Var _ name_) a) b) c) d) e) f_) g ->
            if name == name_ then
                merge a b c d e f_ g

            else
                Nothing

        _ ->
            Nothing


{-| -}
f8 :
    CoreFn.Name
    -> (CoreFn.Expr ann -> CoreFn.Expr ann -> CoreFn.Expr ann -> CoreFn.Expr ann -> CoreFn.Expr ann -> CoreFn.Expr ann -> CoreFn.Expr ann -> CoreFn.Expr ann -> Maybe (CoreFn.Expr ann))
    -> CoreFn.Expr ann
    -> Maybe (CoreFn.Expr ann)
f8 name merge expr =
    case expr of
        CoreFn.App _ (CoreFn.App _ (CoreFn.App _ (CoreFn.App _ (CoreFn.App _ (CoreFn.App _ (CoreFn.App _ (CoreFn.App _ (CoreFn.Var _ name_) a) b) c) d) e) f_) g) h ->
            if name == name_ then
                merge a b c d e f_ g h

            else
                Nothing

        _ ->
            Nothing
