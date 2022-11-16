module CoreFn.Expr exposing
    ( Expr(..)
    , Lit(..), Pat(..)
    , app, lam, let_, lit, pat, var, qual, scoped
    , array, list, con, num, obj, rec, str, bool
    , any, bind, name, val
    , annotation
    , lambdaBindings, letBindings, patternBindings
    , transform, transformMany, transformMany_
    , fold, foldWithAnnotation, foldWithExpr
    , mapAnnotation
    , encode, decoder
    )

{-|


## Types

@docs Expr
@docs Lit, Pat


## Constructors

The following [`Expr`](#Expr) constructors are provided as a convinience for
experimenting with the package, and are unlikely to be useful in any serious
application as they produce expressions with empty annotations!

@docs app, lam, let_, lit, pat, var, qual, scoped


### Literal Constructors

@docs array, list, con, num, obj, rec, str, bool


### Pattern Constructors

@docs any, bind, name, val


## Queries

@docs annotation
@docs lambdaBindings, letBindings, patternBindings


## Manipulations

@docs transform, transformMany, transformMany_
@docs fold, foldWithAnnotation, foldWithExpr
@docs mapAnnotation


## JSON

@docs encode, decoder

-}

-- IMPORTS ---------------------------------------------------------------------

import Basics exposing (Bool(..), Float, Int)
import CoreFn.Name as Name exposing (Name)
import Json.Decode
import Json.Encode
import List
import Maybe exposing (Maybe(..))
import Set exposing (Set)
import String exposing (String)
import Tuple
import Util.Json
import Util.Rec



-- TYPES -----------------------------------------------------------------------


{-| An expression representing a relatively simple functional programming
language. It noteably includes pattern matching and let bindings.

The `ann` type variable represents any annotations you might want to attach to
the AST. This could include source spans to locate or translate the AST back to
some source code, or it could be type annotations added during type inference,
or whatever else might be useful!.

-}
type Expr ann
    = App ann (Expr ann) (Expr ann)
    | Lam ann String (Expr ann)
    | Let ann String (Expr ann) (Expr ann)
    | Lit ann (Lit (Expr ann))
    | Pat ann (Expr ann) (List ( Pat ann, Maybe (Expr ann), Expr ann ))
    | Var ann Name


{-| -}
type Lit expr
    = Arr (List expr)
    | Con String (List expr)
    | Int Int
    | Num Float
    | Obj (List ( String, expr ))
    | Str String


{-| -}
type Pat ann
    = Any ann
    | Bind ann String (Pat ann)
    | Name ann String
    | Val ann (Lit (Pat ann))



-- CONSTANTS -------------------------------------------------------------------
-- CONSTRUCTORS ----------------------------------------------------------------


{-| -}
app : Expr () -> List (Expr ()) -> Expr ()
app fun args =
    case args of
        arg :: rest ->
            app (App () fun arg) rest

        [] ->
            fun


{-| -}
lam : List String -> Expr () -> Expr ()
lam args body =
    case args of
        arg :: rest ->
            Lam () arg (lam rest body)

        [] ->
            body


{-| -}
let_ : List ( String, Expr () ) -> Expr () -> Expr ()
let_ bindings body =
    case bindings of
        ( name_, expr ) :: rest ->
            Let () name_ expr (let_ rest body)

        [] ->
            body


{-| -}
lit : Lit (Expr ()) -> Expr ()
lit =
    Lit ()


{-| -}
array : List expr -> Lit expr
array arr =
    Arr arr


{-| -}
list : List expr -> Lit expr
list =
    array


{-| -}
con : String -> List expr -> Lit expr
con tag args =
    Con tag args


{-| -}
num : Float -> Lit expr
num n =
    Num n


{-| -}
obj : List ( String, expr ) -> Lit expr
obj o =
    Obj o


{-| -}
rec : List ( String, expr ) -> Lit expr
rec =
    obj


{-| -}
str : String -> Lit expr
str s =
    Str s


{-| -}
bool : Bool -> Lit expr
bool b =
    if b then
        con "True" []

    else
        con "False" []


{-| -}
pat : Expr () -> List ( Pat (), Maybe (Expr ()), Expr () ) -> Expr ()
pat expr cases =
    Pat () expr cases


{-| -}
any : Pat ()
any =
    Any ()


{-| -}
bind : String -> Pat () -> Pat ()
bind name_ pattern =
    Bind () name_ pattern


{-| -}
name : String -> Pat ()
name name_ =
    Name () name_


{-| -}
val : Lit (Pat ()) -> Pat ()
val lit_ =
    Val () lit_


{-| -}
var : String -> Expr ()
var name_ =
    Var () (Name.Lower name_)


{-| -}
qual : String -> Name -> Expr ()
qual qualifier name_ =
    Var () (Name.Qualified qualifier name_)


{-| -}
scoped : List String -> Name -> Expr ()
scoped scope name_ =
    Var () (Name.scoped scope name_)



-- QUERIES ---------------------------------------------------------------------


{-| Extract the annotation or metadata attached to an expression. This only works
at a single level. If you want to extract the annotations for subexpressions as
well – to convert into a tree for example – take a look at
[`foldWithAnnotation`](#foldWithAnnotation).
-}
annotation : Expr ann -> ann
annotation expr =
    case expr of
        App ann _ _ ->
            ann

        Lam ann _ _ ->
            ann

        Let ann _ _ _ ->
            ann

        Lit ann _ ->
            ann

        Pat ann _ _ ->
            ann

        Var ann _ ->
            ann


{-| Produces a set of bindings introduced by lambda arguments by walking
consecutive `Lam` expressions and accumulating the arguments. For example,
given...

    \x -> \y -> \z -> ...

...we would produce...

    Set.fromList [ "x", "y", "z" ]

...and...

    \x -> let y = x + 2 in \z -> ...

...would produce...

    Set.fromList [ "x", "z" ]

-}
lambdaBindings : Expr ann -> Set String
lambdaBindings expr =
    case expr of
        Lam _ name_ bod ->
            Set.insert name_ <| lambdaBindings bod

        Let _ _ _ bod ->
            lambdaBindings bod

        _ ->
            Set.empty


{-| Similar to [`lambdaBindings`](#lambdaBindings), this function produces a set
of bindings by walking consecutive `Let` expressions and accumulating the names
introduced. For example, given...

    let x = 1 in
    let y = 2 in
    let z = 3 in
    ...

...we would produce...

        Set.fromList [ "x", "y", "z" ]

-}
letBindings : Expr ann -> Set String
letBindings expr =
    case expr of
        Let _ name_ _ bod ->
            Set.insert name_ <| letBindings bod

        _ ->
            Set.empty


{-| Pattern matching can introduce new bindings into scope the same way lambda
and let expressions might. Not all patterns introduce bindings, though, so this
function tells you what – if any – are introduced!

For example, given...

    [ x, 2, z ] ->
        ...

...we would produce...

    Set.fromList [ "x", "z" ]

...and...

    ( _, { y } ) ->
        ...

...would produce...

    Set.fromList [ "y" ]

-}
patternBindings : Pat ann -> Set String
patternBindings pattern =
    case pattern of
        Any _ ->
            Set.empty

        Bind _ name_ pat_ ->
            Set.insert name_ <| patternBindings pat_

        Name _ name_ ->
            Set.singleton name_

        Val _ (Arr arr) ->
            List.foldl (patternBindings >> Set.union) Set.empty arr

        Val _ (Con _ args) ->
            List.foldl (patternBindings >> Set.union) Set.empty args

        Val _ (Int _) ->
            Set.empty

        Val _ (Num _) ->
            Set.empty

        Val _ (Obj o) ->
            List.foldl (Tuple.second >> patternBindings >> Set.union) Set.empty o

        Val _ (Str _) ->
            Set.empty



-- MANIPULATIONS ---------------------------------------------------------------


{-| Recursively transform an expression, bottom-up.
-}
transform : (Expr ann -> Expr ann) -> Expr ann -> Expr ann
transform f =
    foldWithAnnotation
        { onApp = \ann fun arg -> f <| App ann fun arg
        , onLam = \ann arg bod -> f <| Lam ann arg bod
        , onLet = \ann var_ exp bod -> f <| Let ann var_ exp bod
        , onLit = \ann lit_ -> f <| Lit ann lit_
        , onPat = \ann exp cases -> f <| Pat ann exp cases
        , onVar = \ann name_ -> f <| Var ann name_
        }


{-| Like [`transform`](#transform), this recursively transforms an expression from
the bottom up. It's behaviour is different in two important ways:

  - We supply a _list_ of transformations, that are applied in sequence.
  - Transformations are **continuously applied** until the result does not change.

Transformations are applied to sub-expressions. If you want a non-recursive version
of this function, check out [`transformMany_`](#transformMany_).

🚨 Be mindful of what transformations you are applying, and in what order you
supply them. It is possible to overflow the stack with `transformMany` if the
transformations you supply diverge – we will never stop trying to apply them!

-}
transformMany : List (Expr ann -> Expr ann) -> Expr ann -> Expr ann
transformMany fs =
    let
        applyMany old =
            case List.foldl (<|) old fs of
                new ->
                    if old == new then
                        old

                    else
                        applyMany new
    in
    transform applyMany


{-| Repeatedly apply a list of transformations to a given expression. This is
distinct from [`transformMany`](#transformMany) in that it does **not** apply the
transformations recursively to sub-expressions.

🚨 Be mindful of what transformations you are applying, and in what order you
supply them. It is possible to overflow the stack with `transformMany_` if the
transformations you supply diverge – we will never stop trying to apply them!

-}
transformMany_ : List (Expr ann -> Expr ann) -> Expr ann -> Expr ann
transformMany_ fs =
    let
        applyMany old =
            case List.foldl (<|) old fs of
                new ->
                    if old == new then
                        old

                    else
                        applyMany new
    in
    applyMany


{-| `fold` is an abstraction over a recurisve, bottom-up traversal of an
expression. It takes a record of functions, each of which is responsible for
handling a particular type of expression but where recursive expressions are
replaced with the result of the previous fold. Consider the analog between:

    type Expr ann
        = App ann (Expr ann) (Expr ann)
        | Lam ann String (Expr ann)
        | ...

and

    { onApp : a -> a -> a
    , onLam : a -> String -> a -> a
    , ...
    }

Importantly, the functions we provide to `fold` are **not** recursive!

❓ We mentioned that `fold` performed a _bottom-up_ traversal. You might be
wondering if it is possible to perform a _top-down_ traversal instead... and it
is! To achieve this – and bear with me – we need to accumulate a _function_ from
the fold, and then execute that.

By accumulating a function it is possible to pass state _down_ the tree while
we build something _up_. This is a bit mind bending so let's take a look at a
simple example.

💡 For the true functional programming nerds, this function is also known as a
**catamorphism** or `cata`.

-}
fold :
    { onApp : a -> a -> a
    , onLam : String -> a -> a
    , onLet : String -> a -> a -> a
    , onLit : Lit a -> a
    , onPat : a -> List ( Pat ann, Maybe a, a ) -> a
    , onVar : Name -> a
    }
    -> Expr ann
    -> a
fold fns =
    foldWithExpr
        { onApp = Basics.always fns.onApp
        , onLam = Basics.always fns.onLam
        , onLet = Basics.always fns.onLet
        , onLit = Basics.always fns.onLit
        , onPat = Basics.always fns.onPat
        , onVar = Basics.always fns.onVar
        }


{-| This is a convenient version of the slightly more general
[`foldWithExpr`](#foldWithExpr) that only includes the _annotation_ for the
current expression.
-}
foldWithAnnotation :
    { onApp : ann -> a -> a -> a
    , onLam : ann -> String -> a -> a
    , onLet : ann -> String -> a -> a -> a
    , onLit : ann -> Lit a -> a
    , onPat : ann -> a -> List ( Pat ann, Maybe a, a ) -> a
    , onVar : ann -> Name -> a
    }
    -> Expr ann
    -> a
foldWithAnnotation fns =
    foldWithExpr
        { onApp = annotation >> fns.onApp
        , onLam = annotation >> fns.onLam
        , onLet = annotation >> fns.onLet
        , onLit = annotation >> fns.onLit
        , onPat = annotation >> fns.onPat
        , onVar = annotation >> fns.onVar
        }


{-| Just like how `List.foldl` reduces a list down to a single value, this
function gives us a way to fold an expression tree down to a single value. We
pass in a record of functions that are called according to the type of expression
we are currently folding: you can think of it like pattern matching but now how
the places where our type would typically recurse on `Expr ann` instead have the
type `a` to hold the value of the previous fold.

In addition, we also get the current `Expr ann` being folded – this can be useful
if you need the original context of the expression you are folding. If you just
need the annotation attached to the expression, you can use
[`foldWithAnnotation`](#foldWithAnnotation) instead. If you don't need to know
anything about the expression you are folding, you can use [`fold`](#fold).

💡 For the true functional programming nerds, this function is more commonly
known as a **paramorphism** or `para`. Typically described as a model of _primitive
recursion_, `para` gives us access to the original expression _as well as_ the
transformation that was applied to its child

-}
foldWithExpr :
    { onApp : Expr ann -> a -> a -> a
    , onLam : Expr ann -> String -> a -> a
    , onLet : Expr ann -> String -> a -> a -> a
    , onLit : Expr ann -> Lit a -> a
    , onPat : Expr ann -> a -> List ( Pat ann, Maybe a, a ) -> a
    , onVar : Expr ann -> Name -> a
    }
    -> Expr ann
    -> a
foldWithExpr fns =
    Util.Rec.run
        (\expr ->
            -- ❗️ Make sure you have configured your editor to use the local
            -- wrapper for `elm-format` provided in `./scripts/elm-format.js`.
            --
            -- This does some trickery to prevent the nested lambdas from
            -- exploding with indentation and is a significant QOL and readability
            -- buff!
            case expr of
                App _ fun arg ->
                    Util.Rec.andRec fun <| \f ->
                    Util.Rec.andRec arg <| \a ->
                    Util.Rec.base <| fns.onApp expr f a

                Lam _ arg bod ->
                    Util.Rec.andRec bod <| \b ->
                    Util.Rec.base <| fns.onLam expr arg b

                Let _ name_ exp bod ->
                    Util.Rec.andRec exp <| \e ->
                    Util.Rec.andRec bod <| \b ->
                    Util.Rec.base <| fns.onLet expr name_ e b

                Lit _ (Arr arr) ->
                    Util.Rec.andFold (::) [] arr <| \a ->
                    Util.Rec.base <| fns.onLit expr (Arr a)

                Lit _ (Con tag args) ->
                    Util.Rec.andFold (::) [] args <| \a ->
                    Util.Rec.base <| fns.onLit expr (Con tag a)

                Lit _ (Int int) ->
                    Util.Rec.base <| fns.onLit expr (Int int)

                Lit _ (Num n) ->
                    Util.Rec.base <| fns.onLit expr (Num n)

                Lit _ (Obj o) ->
                    let
                        recField ( key, val_ ) =
                            Util.Rec.andRec val_ <| \v ->
                            Util.Rec.base ( key, v )
                    in
                    Util.Rec.andTraverse recField o <| \o_ ->
                    Util.Rec.base <| fns.onLit expr (Obj o_)

                Lit _ (Str s) ->
                    Util.Rec.base <| fns.onLit expr (Str s)

                Pat _ exp cases ->
                    let
                        recCase ( pat_, maybeGrd, bod ) =
                            case maybeGrd of
                                Just grd ->
                                    Util.Rec.andRec grd <| \g ->
                                    Util.Rec.andRec bod <| \b ->
                                    Util.Rec.base ( pat_, Just g, b )

                                Nothing ->
                                    Util.Rec.andRec bod <| \b ->
                                    Util.Rec.base ( pat_, Nothing, b )
                    in
                    Util.Rec.andRec exp <| \e ->
                    Util.Rec.andTraverse recCase cases <| \c ->
                    Util.Rec.base <| fns.onPat expr e c

                Var _ var_ ->
                    Util.Rec.base <| fns.onVar expr var_
        )


{-| -}
mapAnnotation : (a -> b) -> Expr a -> Expr b
mapAnnotation f =
    let
        mapPatAnnotation pattern =
            case pattern of
                Any ann ->
                    Any (f ann)

                Bind ann name_ subpat ->
                    Bind (f ann) name_ (mapPatAnnotation subpat)

                Name ann name_ ->
                    Name (f ann) name_

                Val ann (Arr arr) ->
                    Val (f ann) (Arr <| List.map mapPatAnnotation arr)

                Val ann (Con tag args) ->
                    Val (f ann) (Con tag <| List.map mapPatAnnotation args)

                Val ann (Int int) ->
                    Val (f ann) (Int int)

                Val ann (Num n) ->
                    Val (f ann) (Num n)

                Val ann (Obj o) ->
                    Val (f ann) (Obj <| List.map (Tuple.mapSecond mapPatAnnotation) o)

                Val ann (Str s) ->
                    Val (f ann) (Str s)
    in
    foldWithAnnotation
        { onApp = \ann fun arg -> App (f ann) fun arg
        , onLam = \ann arg exp -> Lam (f ann) arg exp
        , onLet = \ann var_ exp bod -> Let (f ann) var_ exp bod
        , onLit = \ann lit_ -> Lit (f ann) lit_
        , onPat = \ann exp cases -> Pat (f ann) exp (List.map (\( pattern, grd, bod ) -> ( mapPatAnnotation pattern, grd, bod )) cases)
        , onVar = \ann var_ -> Var (f ann) var_
        }



-- JSON ------------------------------------------------------------------------


{-| -}
encode : (ann -> Json.Encode.Value) -> Expr ann -> Json.Encode.Value
encode encodeAnnotation =
    foldWithAnnotation
        { onApp =
            \ann fun arg ->
                Util.Json.encodeTaggedValue "App"
                    [ ( "ann", encodeAnnotation ann )
                    , ( "fun", fun )
                    , ( "arg", arg )
                    ]
        , onLam =
            \ann arg exp ->
                Util.Json.encodeTaggedValue "Lam"
                    [ ( "ann", encodeAnnotation ann )
                    , ( "arg", Json.Encode.string arg )
                    , ( "exp", exp )
                    ]
        , onLet =
            \ann var_ exp bod ->
                Util.Json.encodeTaggedValue "Let"
                    [ ( "ann", encodeAnnotation ann )
                    , ( "var", Json.Encode.string var_ )
                    , ( "exp", exp )
                    , ( "bod", bod )
                    ]
        , onLit =
            \ann lit_ ->
                Util.Json.encodeTaggedValue "Lit"
                    [ ( "ann", encodeAnnotation ann )
                    , ( "lit", encodeLit Basics.identity lit_ )
                    ]
        , onPat =
            \ann exp cases ->
                Util.Json.encodeTaggedValue "Pat"
                    [ ( "ann", encodeAnnotation ann )
                    , ( "exp", exp )
                    , ( "cases", Json.Encode.list (encodeCase encodeAnnotation) cases )
                    ]
        , onVar =
            \ann var_ ->
                Util.Json.encodeTaggedValue "Var"
                    [ ( "ann", encodeAnnotation ann )
                    , ( "var", Name.encode var_ )
                    ]
        }


encodeLit : (expr -> Json.Encode.Value) -> Lit expr -> Json.Encode.Value
encodeLit encodeExpr lit_ =
    let
        encodeField ( key, val_ ) =
            Json.Encode.list Basics.identity
                [ Json.Encode.string key
                , encodeExpr val_
                ]
    in
    case lit_ of
        Arr arr ->
            Util.Json.encodeTaggedValue "Arr"
                [ ( "arr", Json.Encode.list encodeExpr arr ) ]

        Con tag args ->
            Util.Json.encodeTaggedValue "Con"
                [ ( "tag", Json.Encode.string tag )
                , ( "args", Json.Encode.list encodeExpr args )
                ]

        Int int ->
            Util.Json.encodeTaggedValue "Int"
                [ ( "int", Json.Encode.int int ) ]

        Num n ->
            Util.Json.encodeTaggedValue "Num"
                [ ( "n", Json.Encode.float n ) ]

        Obj o ->
            Util.Json.encodeTaggedValue "Obj"
                [ ( "o", Json.Encode.list encodeField o ) ]

        Str s ->
            Util.Json.encodeTaggedValue "Str"
                [ ( "s", Json.Encode.string s ) ]


encodeCase : (ann -> Json.Encode.Value) -> ( Pat ann, Maybe Json.Encode.Value, Json.Encode.Value ) -> Json.Encode.Value
encodeCase encodeAnnotation ( pattern, guard, body ) =
    Json.Encode.list Basics.identity
        [ encodePattern encodeAnnotation pattern
        , Maybe.withDefault Json.Encode.null guard
        , body
        ]


encodePattern : (ann -> Json.Encode.Value) -> Pat ann -> Json.Encode.Value
encodePattern encodeAnnotation pattern =
    case pattern of
        Any ann ->
            Util.Json.encodeTaggedValue "Any"
                [ ( "ann", encodeAnnotation ann ) ]

        Bind ann name_ pat_ ->
            Util.Json.encodeTaggedValue "Bind"
                [ ( "ann", encodeAnnotation ann )
                , ( "name", Json.Encode.string name_ )
                , ( "pattern", encodePattern encodeAnnotation pat_ )
                ]

        Name ann name_ ->
            Util.Json.encodeTaggedValue "Name"
                [ ( "ann", encodeAnnotation ann )
                , ( "name", Json.Encode.string name_ )
                ]

        Val ann lit_ ->
            Util.Json.encodeTaggedValue "Val"
                [ ( "ann", encodeAnnotation ann )
                , ( "lit", encodeLit (encodePattern encodeAnnotation) lit_ )
                ]


{-| -}
decoder : Json.Decode.Decoder ann -> Json.Decode.Decoder (Expr ann)
decoder annotationDecoder =
    Util.Json.taggedValueDecoder
        (\tag ->
            case tag of
                "App" ->
                    Json.Decode.map3 App
                        (Json.Decode.field "ann" <| annotationDecoder)
                        (Json.Decode.field "fun" <| decoder annotationDecoder)
                        (Json.Decode.field "arg" <| decoder annotationDecoder)

                "Lam" ->
                    Json.Decode.map3 Lam
                        (Json.Decode.field "ann" <| annotationDecoder)
                        (Json.Decode.field "arg" <| Json.Decode.string)
                        (Json.Decode.field "exp" <| decoder annotationDecoder)

                "Let" ->
                    Json.Decode.map4 Let
                        (Json.Decode.field "ann" <| annotationDecoder)
                        (Json.Decode.field "var" <| Json.Decode.string)
                        (Json.Decode.field "exp" <| decoder annotationDecoder)
                        (Json.Decode.field "bod" <| decoder annotationDecoder)

                "Lit" ->
                    Json.Decode.map2 Lit
                        (Json.Decode.field "ann" <| annotationDecoder)
                        (Json.Decode.field "lit" <| (litDecoder <| decoder annotationDecoder))

                "Pat" ->
                    Json.Decode.map3 Pat
                        (Json.Decode.field "ann" <| annotationDecoder)
                        (Json.Decode.field "exp" <| decoder annotationDecoder)
                        (Json.Decode.field "cases" <| Json.Decode.list <| caseDecoder annotationDecoder)

                "Var" ->
                    Json.Decode.map2 Var
                        (Json.Decode.field "ann" <| annotationDecoder)
                        (Json.Decode.field "var" <| Name.decoder)

                _ ->
                    Json.Decode.fail <| "[CoreFn.Expr.decoder] Unknown tag: " ++ tag
        )


litDecoder : Json.Decode.Decoder expr -> Json.Decode.Decoder (Lit expr)
litDecoder exprDecoder =
    let
        fieldDecoder =
            Json.Decode.map2 Tuple.pair
                (Json.Decode.index 0 <| Json.Decode.string)
                (Json.Decode.index 1 <| exprDecoder)
    in
    Util.Json.taggedValueDecoder
        (\tag ->
            case tag of
                "Arr" ->
                    Json.Decode.map Arr
                        (Json.Decode.field "arr" <| Json.Decode.list exprDecoder)

                "Con" ->
                    Json.Decode.map2 Con
                        (Json.Decode.field "tag" <| Json.Decode.string)
                        (Json.Decode.field "args" <| Json.Decode.list exprDecoder)

                "Int" ->
                    Json.Decode.map Int
                        (Json.Decode.field "int" <| Json.Decode.int)

                "Num" ->
                    Json.Decode.map Num
                        (Json.Decode.field "n" <| Json.Decode.float)

                "Obj" ->
                    Json.Decode.map Obj
                        (Json.Decode.field "o" <| Json.Decode.list fieldDecoder)

                "Str" ->
                    Json.Decode.map Str
                        (Json.Decode.field "s" <| Json.Decode.string)

                _ ->
                    Json.Decode.fail <| "[CoreFn.Expr.litDecoder] Unknown tag: " ++ tag
        )


caseDecoder : Json.Decode.Decoder ann -> Json.Decode.Decoder ( Pat ann, Maybe (Expr ann), Expr ann )
caseDecoder annotationDecoder =
    Json.Decode.map3 (\pat_ grd bod -> ( pat_, grd, bod ))
        (Json.Decode.index 0 <| patternDecoder annotationDecoder)
        (Json.Decode.index 1 <| Json.Decode.nullable <| decoder annotationDecoder)
        (Json.Decode.index 2 <| decoder annotationDecoder)


patternDecoder : Json.Decode.Decoder ann -> Json.Decode.Decoder (Pat ann)
patternDecoder annotationDecoder =
    Util.Json.taggedValueDecoder
        (\tag ->
            case tag of
                "Any" ->
                    Json.Decode.map Any
                        (Json.Decode.field "ann" <| annotationDecoder)

                "Bind" ->
                    Json.Decode.map3 Bind
                        (Json.Decode.field "ann" <| annotationDecoder)
                        (Json.Decode.field "name" <| Json.Decode.string)
                        (Json.Decode.field "pattern" <| patternDecoder annotationDecoder)

                "Name" ->
                    Json.Decode.map2 Name
                        (Json.Decode.field "ann" <| annotationDecoder)
                        (Json.Decode.field "name" <| Json.Decode.string)

                "Val" ->
                    Json.Decode.map2 Val
                        (Json.Decode.field "ann" <| annotationDecoder)
                        (Json.Decode.field "lit" <| litDecoder <| patternDecoder annotationDecoder)

                _ ->
                    Json.Decode.fail <| "[CoreFn.Expr.patternDecoder] Unknown tag: " ++ tag
        )



-- UTILS -----------------------------------------------------------------------
