module NoSinglePatternCaseTest exposing (all)

import NoSinglePatternCase exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoSinglePatternCase"
        [ allowed, disallowed ]


disallowed : Test
disallowed =
    describe "does not allow"
        [ withUselessPattern
        , withPattern
        ]


withUselessPattern : Test
withUselessPattern =
    describe "with useless pattern (_ or ())"
        [ describe "replace argument"
            [ test "_" <|
                \() ->
                    """module A exposing (..)

always2 : a -> Int
always2 a =
    case a of
        _ -> 2
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ error """case a of
        _ -> 2""" |> Review.Test.whenFixed """module A exposing (..)

always2 : a -> Int
always2 _ =
    2
"""
                            ]
            , test "()" <|
                \() ->
                    """module A exposing (..)

pointless : () -> Bool
pointless unit =
    case unit of
        () -> True
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ error """case unit of
        () -> True"""
                                |> Review.Test.whenFixed """module A exposing (..)

pointless : () -> Bool
pointless () =
    True
"""
                            ]
            ]
        , test "can't replace argument" <|
            \() ->
                """module A exposing (..)

add2 : number -> number
add2 n =
    case n of
        _ -> n + 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ error """case n of
        _ -> n + 2""" |> Review.Test.whenFixed """module A exposing (..)

add2 : number -> number
add2 n =
    n + 2
"""
                        ]
        ]


withPattern : Test
withPattern =
    describe "with pattern"
        [ describe "replace argument"
            [ test "destructure named pattern" <|
                \() ->
                    """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    case o of
        Opaque i -> i
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ error """case o of
        Opaque i -> i""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack (Opaque i) =
    i
"""
                            ]
            , test "var pattern in destructuring let" <|
                \() ->
                    """module A exposing (..)

type Opaque a = Opaque a

unpack : Opaque a -> a
unpack ((Opaque ii) as oo) =
    let
        (Opaque o) =
            Opaque oo

        subUnpack =
            case o of
                Opaque i -> i
    in
    subUnpack
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ error """case o of
                Opaque i -> i""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque a = Opaque a

unpack : Opaque a -> a
unpack ((Opaque ii) as oo) =
    let
        (Opaque (Opaque i)) =
            Opaque oo

        subUnpack =
            i
    in
    subUnpack
"""
                            ]
            ]
        , describe "can't replace argument"
            [ test "because the variable in case and of is used multiple times" <|
                \() ->
                    """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> ( Int, Opaque )
unpack o =
    case o of
        Opaque i -> ( i, o )
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ error """case o of
        Opaque i -> ( i, o )""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> ( Int, Opaque )
unpack o =
    let
        (Opaque i) =
            o
    in
    ( i, o )
"""
                            ]
            , test "lets are not combined" <|
                \() ->
                    """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    let
        (Opaque ii) =
            o
    in
    case o of
        Opaque i -> i
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ error """case o of
        Opaque i -> i""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    let
        (Opaque ii) =
            o
    in
    let
        (Opaque i) =
            o
    in
    i
"""
                            ]
            , test "because in case and of isn't just a variable" <|
                \() ->
                    """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    case ( o, o ) of
        ( Opaque i, Opaque ii ) -> i
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ error """case ( o, o ) of
        ( Opaque i, Opaque ii ) -> i""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    let
        ( Opaque i, Opaque ii ) =
            ( o, o )
    in
    i
"""
                            ]
            , test "because the variable in case and of is defined at top level" <|
                \() ->
                    """module A exposing (..)

topLevel : Opaque
topLevel =
    Opaque 2

type Opaque = Opaque Int

unpack : Int
unpack =
    case topLevel of
        Opaque i -> i
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ error """case topLevel of
        Opaque i -> i""" |> Review.Test.whenFixed """module A exposing (..)

topLevel : Opaque
topLevel =
    Opaque 2

type Opaque = Opaque Int

unpack : Int
unpack =
    let
        (Opaque i) =
            topLevel
    in
    i
"""
                            ]
            , test "because the variable in case and of is from a pattern after as" <|
                \() ->
                    """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack ((Opaque ii) as o) =
    case o of
        Opaque i -> i
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ error """case o of
        Opaque i -> i""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack ((Opaque ii) as o) =
    let
        (Opaque i) =
            o
    in
    i
"""
                            ]
            , test "because the variable in case and of is from a record field pattern" <|
                \() ->
                    """module A exposing (..)

type Opaque = Opaque Int

unpack : { o : Opaque } -> Int
unpack { o } =
    case o of
        Opaque i -> i
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ error """case o of
        Opaque i -> i""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : { o : Opaque } -> Int
unpack { o } =
    let
        (Opaque i) =
            o
    in
    i
"""
                            ]
            , test "because the variable in case and of is the name of a let function with a annotation" <|
                \() ->
                    """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack oo =
    let
        o : Opaque
        o =
            oo
    in
    let
        unpacked =
            case o of
                Opaque i -> i
    in
    unpacked
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ error """case o of
                Opaque i -> i""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack oo =
    let
        o : Opaque
        o =
            oo

        unpacked =
            let
                (Opaque i) =
                    o
            in
            i
    in
    unpacked
"""
                            ]
            ]
        ]


allowed : Test
allowed =
    describe "allows"
        [ test "multi-pattern cases" <|
            \() ->
                """module A exposing (..)

type AOrB = A | B

isA : AOrB -> Bool
isA aOrB =
    case aOrB of
        A -> True
        B -> False
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]


error : String -> Review.Test.ExpectedError
error under =
    Review.Test.error
        { message = "Single pattern case block."
        , details = [ "Single pattern case blocks are either unnecessary or overly verbose.  There's usually a more concise way to destructure, e.g. in a function argument, so consider refactoring." ]
        , under = under
        }
