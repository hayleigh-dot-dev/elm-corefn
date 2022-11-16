module Util.Rec exposing (..)

{-| This module is copied with very little modification from
<https://github.com/micahhahn/elm-safe-recursion/tree/2.0.0>. That module is
licensed under the BSD 3-Clause License, which is reproduced below:

BSD 3-Clause License

Copyright (c) 2022, NoRedInk
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.

  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

  - Neither the name of the copyright holder nor the names of its
    contributors may be used to endorse or promote products derived from
    this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

-- TYPES -----------------------------------------------------------------------


type Rec r t a
    = Rec r (t -> Rec r t a)
    | Base a



-- CONSTRUCTORS ----------------------------------------------------------------


base : a -> Rec r t a
base a =
    Base a


rec : r -> Rec r t t
rec r =
    Rec r base



-- MANIPULATIONS ---------------------------------------------------------------


map : (a -> b) -> Rec r t a -> Rec r t b
map f step =
    case step of
        Base t ->
            Base (f t)

        Rec r cont ->
            Rec r (\a -> cont a |> map f)


map2 : (a -> b -> c) -> Rec r t a -> Rec r t b -> Rec r t c
map2 f step1 step2 =
    andThen (\a -> map (f a) step2) step1


andThen : (a -> Rec r t b) -> Rec r t a -> Rec r t b
andThen f step =
    case step of
        Base t ->
            f t

        Rec r cont ->
            Rec r (\a -> cont a |> andThen f)


andRec : r -> (t -> Rec r t a) -> Rec r t a
andRec =
    Rec


fold : (t -> a -> a) -> a -> List r -> Rec r t a
fold f accum items =
    andFold f accum items base


andFold : (t -> a -> a) -> a -> List r -> (a -> Rec r t b) -> Rec r t b
andFold f acc items step =
    case items of
        [] ->
            step acc

        item :: rest ->
            andRec item (\t -> andFold f (f t acc) rest step)


traverse : (x -> Rec r t a) -> List x -> Rec r t (List a)
traverse project items =
    andTraverse project items base


andTraverse : (x -> Rec r t a) -> List x -> (List a -> Rec r t b) -> Rec r t b
andTraverse proj items step =
    let
        go acc todo =
            case todo of
                [] ->
                    step <| List.reverse acc

                item :: rest ->
                    proj item |> andThen (\a -> go (a :: acc) rest)
    in
    go [] items



-- CONVERSIONS -----------------------------------------------------------------


run : (r -> Rec r t t) -> r -> t
run proj init =
    let
        go step stack =
            case step of
                Base t ->
                    case stack of
                        [] ->
                            t

                        next :: rest ->
                            go (next t) rest

                Rec r cont ->
                    go (proj r) (cont :: stack)
    in
    go (proj init) []



-- UTILS -----------------------------------------------------------------------
