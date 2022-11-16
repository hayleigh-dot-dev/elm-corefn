# CoreFn

> The minimal core of a functional programming language.

## What is this?

Central to the `CoreFn` package is the following expression type:

```elm
type Expr ann
    = App ann (Expr ann) (Expr ann)
    | Lam ann String (Expr ann)
    | Let ann String (Expr ann) (Expr ann)
    | Lit ann (Lit (Expr ann))
    | Pat ann (Expr ann) (List ( Pat ann, Maybe (Expr ann), Expr ann ))
    | Var ann Name
```

This expression type is a fairly minimal, but feature-complete representation
of a functional programming language like Elm. For those into this kind of thing,
you can think of the `CoreFn` as a kind of lambda calculus with pattern matching
and let binding.

## Who is it for?

This package is mainly used in the [Ren](https://github.com/ren-lang/compiler)
compiler as an intermediate representation (IR) after parsing. As it happens,
many high-level language features can be defined in terms of the `CoreFn` IR,
and doing so often simplifies optimisation and type checking passes.

Although designed to be used as part of Ren's compiler, the mid-level nature of
the `CoreFn` IR means it can be useful in other compiler projects written in Elm
as well.

If you're interested in programming language development in Elm, making us of this
package is a good way to get started: just write a parser and you're already half
way done! You may then want to translate the `CoreFn` IR to a representation
closer to a target language like JavaScript or Python. For that you can check out
the companion package [`CoreImp`](https://package.elm-lang.org/packages/hayleigh-dot-dev/elm-coreimp/latest).

## How do I use it?

If you have an existing programming language project you've been working on, you
can consider translating to the `CoreFn` IR as part of your compiler pipeline.
Doing so would let you access the suite of optimisation passes made available,
including constant folding, dead code elimination, and common subexpression
elimination.

If you're starting a new project, you can use this package as a starting point
by writing a parser your surface syntax and parsing directly into the `CoreFn`
IR. From there you can look at emitting JavaScript or some other target, or
you could write a tree-walking interpreter for the `CoreFn` IR itself.
