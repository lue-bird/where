module Ns exposing (Add31, Add32, Add62, Add64, N31, N32, N62, N63, N64, n31, n32, n62, n63, n64)

{-| When you need many small-ish numbers or a few medium sized ones.
-}

import N exposing (Diff, In, Is, N, N0able, To, diffAdd, n1, n16, n2, n4, n8)
import Possibly exposing (Possibly)


{-| Type for the natural number `64 +` some natural number `n`
-}
type alias Add64 n =
    N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able n Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never


{-| Type for the exact natural number `64`
-}
type alias N64 =
    N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able Never Possibly) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never


{-| The exact natural number `64`
-}
n64 : N (In N64 (Add64 (N0able atLeast_ Possibly)) (Is (Diff x0 To (Add64 x0)) (Diff x1 To (Add64 x1))))
n64 =
    n32 |> diffAdd ( n32, n32 )


{-| Type for the natural number `32 +` some natural number `n`
-}
type alias Add32 n =
    N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able n Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never


{-| Type for the exact natural number `32`
-}
type alias N32 =
    N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able Never Possibly) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never


{-| The exact natural number `32`
-}
n32 : N (In N32 (Add32 (N0able atLeast_ Possibly)) (Is (Diff x0 To (Add32 x0)) (Diff x1 To (Add32 x1))))
n32 =
    n16 |> diffAdd ( n16, n16 )


{-| Type for the natural number `63 +` some natural number `n`
-}
type alias Add63 n =
    N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able n Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never


{-| Type for the exact natural number `63`
-}
type alias N63 =
    N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able Never Possibly) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never


{-| The exact natural number `63`
-}
n63 : N (In N63 (Add63 (N0able atLeast_ Possibly)) (Is (Diff x0 To (Add63 x0)) (Diff x1 To (Add63 x1))))
n63 =
    n32
        |> diffAdd ( n16, n16 )
        |> diffAdd ( n8, n8 )
        |> diffAdd ( n4, n4 )
        |> diffAdd ( n2, n2 )
        |> diffAdd ( n1, n1 )


{-| Type for the natural number `31 +` some natural number `n`
-}
type alias Add31 n =
    N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able n Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never


{-| Type for the exact natural number `31`
-}
type alias N31 =
    N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able Never Possibly) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never


{-| The exact natural number `31`
-}
n31 : N (In N31 (Add31 (N0able atLeast_ Possibly)) (Is (Diff x0 To (Add31 x0)) (Diff x1 To (Add31 x1))))
n31 =
    n16
        |> diffAdd ( n8, n8 )
        |> diffAdd ( n4, n4 )
        |> diffAdd ( n2, n2 )
        |> diffAdd ( n1, n1 )


{-| Type for the natural number `62 +` some natural number `n`
-}
type alias Add62 n =
    N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able n Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never


{-| Type for the exact natural number `62`
-}
type alias N62 =
    N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able (N0able Never Possibly) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never) Never


{-| The exact natural number `62`
-}
n62 : N (In N62 (Add62 (N0able atLeast_ Possibly)) (Is (Diff x0 To (Add62 x0)) (Diff x1 To (Add62 x1))))
n62 =
    n32
        |> diffAdd ( n16, n16 )
        |> diffAdd ( n8, n8 )
        |> diffAdd ( n4, n4 )
        |> diffAdd ( n2, n2 )
