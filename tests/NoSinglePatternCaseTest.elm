module NoSinglePatternCaseTest exposing (all)

import NoSinglePatternCase
    exposing
        ( andIfAsPatternRequired
        , andIfCannotDestructureAtArgument
        , andIfNoLetExists
        , createNewLet
        , fail
        , fixInArgument
        , fixInArgumentInstead
        , fixInLet
        , fixInLetInstead
        , ifAsPatternRequired
        , ifCannotDestructureAtArgument
        , ifNoLetExists
        , replaceUnusedBindings
        , rule
        , useAsPattern
        )
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoSinglePatternCase"
        [ allowed, disallowed ]


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
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectNoErrors
        ]


disallowed : Test
disallowed =
    describe "does not allow"
        [ uselessPattern
        , fixInArgSuite
        , fixInLetSuite
        ]


uselessPattern : Test
uselessPattern =
    describe "useless pattern"
        [ test "wildcard" <|
            \() ->
                """module A exposing (..)

always2 : a -> Int
always2 a =
    case a of
        _ -> 2
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error """case a of
        _ -> 2""" |> Review.Test.whenFixed """module A exposing (..)

always2 : a -> Int
always2 a =
    2
"""
                        ]
        , test "unit" <|
            \() ->
                """module A exposing (..)

pointless : () -> Bool
pointless unit =
    case unit of
        () -> True
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error """case unit of
        () -> True""" |> Review.Test.whenFixed """module A exposing (..)

pointless : () -> Bool
pointless unit =
    True
"""
                        ]
        , test "complex pattern" <|
            \() ->
                """module A exposing (..)

pointless : { n : a } -> Bool
pointless record =
    case record of
        ({ n } as r) -> True
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error """case record of
        ({ n } as r) -> True""" |> Review.Test.whenFixed """module A exposing (..)

pointless : { n : a } -> Bool
pointless record =
    True
"""
                        ]
        , test "single custom type" <|
            \() ->
                """module A exposing (..)

type A = A Int

pointless : A -> Bool
pointless a =
    case a of
        A int -> True
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error """case a of
        A int -> True""" |> Review.Test.whenFixed """module A exposing (..)

type A = A Int

pointless : A -> Bool
pointless a =
    True
"""
                        ]
        , replaceUnusedSuite
        ]


replaceUnusedSuite : Test
replaceUnusedSuite =
    describe "replace unused bindings"
        [ test "wildcard" <|
            \() ->
                """module A exposing (..)

always2 : a -> Int
always2 a =
    case a of
        _ -> 2
"""
                    |> Review.Test.run
                        (fixInArgument
                            |> replaceUnusedBindings
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error """case a of
        _ -> 2""" |> Review.Test.whenFixed """module A exposing (..)

always2 : a -> Int
always2 _ =
    2
"""
                        ]
        , test "unit" <|
            \() ->
                """module A exposing (..)

pointless : () -> Bool
pointless unit =
    case unit of
        () -> True
"""
                    |> Review.Test.run
                        (fixInArgument
                            |> replaceUnusedBindings
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error """case unit of
        () -> True""" |> Review.Test.whenFixed """module A exposing (..)

pointless : () -> Bool
pointless _ =
    True
"""
                        ]
        , test "complex pattern" <|
            \() ->
                """module A exposing (..)

pointless : { n : a } -> Bool
pointless record =
    case record of
        ({ n } as r) -> True
"""
                    |> Review.Test.run
                        (fixInArgument
                            |> replaceUnusedBindings
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error """case record of
        ({ n } as r) -> True""" |> Review.Test.whenFixed """module A exposing (..)

pointless : { n : a } -> Bool
pointless _ =
    True
"""
                        ]
        , test "single custom type" <|
            \() ->
                """module A exposing (..)

type A = A Int

pointless : A -> Bool
pointless a =
    case a of
        A int -> True
"""
                    |> Review.Test.run
                        (fixInArgument
                            |> replaceUnusedBindings
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error """case a of
        A int -> True""" |> Review.Test.whenFixed """module A exposing (..)

type A = A Int

pointless : A -> Bool
pointless _ =
    True
"""
                        ]
        , test "nested custom types" <|
            \() ->
                """module A exposing (..)

type A = A Int

type B = B A A

pointless : B -> Bool
pointless b =
    case b of
        B (A i1) (A i2) -> True
"""
                    |> Review.Test.run
                        (fixInArgument
                            |> replaceUnusedBindings
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error """case b of
        B (A i1) (A i2) -> True""" |> Review.Test.whenFixed """module A exposing (..)

type A = A Int

type B = B A A

pointless : B -> Bool
pointless _ =
    True
"""
                        ]
        , test "record field cannot be fixed" <|
            \() ->
                """module A exposing (..)

pointless : {a : Int, b : Int} -> Bool
pointless {a, b} =
    case (a, b) of
        (_, _) -> True
"""
                    |> Review.Test.run
                        (fixInArgument
                            |> replaceUnusedBindings
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error """case (a, b) of
        (_, _) -> True""" |> Review.Test.whenFixed """module A exposing (..)

pointless : {a : Int, b : Int} -> Bool
pointless {a, b} =
    True
"""
                        ]
        , test "let declaration" <|
            \() ->
                """module A exposing (..)

type A = A Int

pointless : A -> Bool
pointless a =
    let
        b = a
    in
    case b of
        A int -> True
"""
                    |> Review.Test.run
                        (fixInArgument
                            |> replaceUnusedBindings
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error """case b of
        A int -> True""" |> Review.Test.whenFixed """module A exposing (..)

type A = A Int

pointless : A -> Bool
pointless a =
    let
        _ = a
    in
    True
"""
                        ]
        , test "multiple bindings" <|
            \() ->
                """module A exposing (..)

pointless : Int -> Int -> Int -> Bool
pointless a b c =
    let
        foo = b
    in
    case (a, b, c) of
        (_, _, _) -> True
"""
                    |> Review.Test.run
                        (fixInArgument
                            |> replaceUnusedBindings
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error """case (a, b, c) of
        (_, _, _) -> True""" |> Review.Test.whenFixed """module A exposing (..)

pointless : Int -> Int -> Int -> Bool
pointless _ b _ =
    let
        foo = b
    in
    True
"""
                        ]
        ]


fixInArgSuite : Test
fixInArgSuite =
    describe "fixes in argument"
        [ test "simple case" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    case o of
        Opaque i -> i
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error """case o of
        Opaque i -> i""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack (Opaque i) =
    i
"""
                        ]
        , test "expression contains binding multiple times" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    case o of
        Opaque i -> i + i
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error """case o of
        Opaque i -> i + i""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack (Opaque i) =
    i + i
"""
                        ]
        , test "in let declaration" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack =
    let
        f o =
            case o of
                Opaque i -> i
    in
    f
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error """case o of
                Opaque i -> i""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack =
    let
        f (Opaque i) =
            i
    in
    f
"""
                        ]
        , test "in lambda" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack =
    \\o ->
        case o of
            Opaque i -> i
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error """case o of
            Opaque i -> i""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack =
    \\(Opaque i) ->
        i
"""
                        ]
        , asPatternSuite
        , cannotDestructureInArgSuite
        ]


asPatternSuite : Test
asPatternSuite =
    describe "as pattern"
        [ test "does not use as when name is used but does not refer to binding and is multiline expression" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

withUnpacked : Opaque -> ( Int, List Int )
withUnpacked map =
    case map of
        Opaque i ->
            ( i
            , List.map ((+) 1) [ 1 ]
            )
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error """case map of
        Opaque i ->
            ( i
            , List.map ((+) 1) [ 1 ]
            )"""
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

withUnpacked : Opaque -> ( Int, List Int )
withUnpacked (Opaque i) =
    ( i
            , List.map ((+) 1) [ 1 ]
            )
"""
                        ]
        , test "does not use as when name is used outside of scope" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

f : Opaque -> Bool
f x =
    let
        otherFunc a =
            a + 1

        unpack a =
            case a of
                Opaque i -> i
    in
    unpack x
        |> otherFunc
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error """case a of
                Opaque i -> i"""
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

f : Opaque -> Bool
f x =
    let
        otherFunc a =
            a + 1

        unpack (Opaque i) =
            i
    in
    unpack x
        |> otherFunc
"""
                        ]
        , test "uses when the var is used in the expression" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

withUnpacked : Opaque -> ( Int, Opaque )
withUnpacked o =
    case o of
        Opaque i -> ( i, o )
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error """case o of
        Opaque i -> ( i, o )""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

withUnpacked : Opaque -> ( Int, Opaque )
withUnpacked ((Opaque i) as o) =
    ( i, o )
""" ]
        , test "works in case patterns" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

type AOrB = A Opaque | B Opaque

withUnpacked : AOrB -> ( Int, Opaque )
withUnpacked aOrB =
    case aOrB of
        A o ->
            case o of
                Opaque i -> ( i + 1, o )
        B o ->
            case o of
                Opaque i -> ( i - 1, o )
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error """case o of
                Opaque i -> ( i + 1, o )"""
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type Opaque = Opaque Int

type AOrB = A Opaque | B Opaque

withUnpacked : AOrB -> ( Int, Opaque )
withUnpacked aOrB =
    case aOrB of
        A ((Opaque i) as o) ->
            ( i + 1, o )
        B o ->
            case o of
                Opaque i -> ( i - 1, o )
"""
                        , error """case o of
                Opaque i -> ( i - 1, o )"""
                            |> Review.Test.whenFixed
                                """module A exposing (..)

type Opaque = Opaque Int

type AOrB = A Opaque | B Opaque

withUnpacked : AOrB -> ( Int, Opaque )
withUnpacked aOrB =
    case aOrB of
        A o ->
            case o of
                Opaque i -> ( i + 1, o )
        B ((Opaque i) as o) ->
            ( i - 1, o )
"""
                        ]
        , test "nested in let" <|
            \() ->
                """module A exposing (..)

type Opaque a = Opaque a

unpack : Opaque a -> a
unpack ((Opaque ii) as oo) =
    let
        (Opaque o) =
            Opaque oo

        unpacked =
            case o of
                Opaque i -> i
    in
    unpacked
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error """case o of
                Opaque i -> i""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque a = Opaque a

unpack : Opaque a -> a
unpack ((Opaque ii) as oo) =
    let
        (Opaque (Opaque i)) =
            Opaque oo

        unpacked =
            i
    in
    unpacked
"""
                        ]
        , asFallbackSuite
        ]


asFallbackSuite : Test
asFallbackSuite =
    describe "falls back from as pattern"
        [ test "fails" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

withUnpacked : Opaque -> ( Int, Opaque )
withUnpacked o =
    let
        foo =
            bar
    in
    case o of
        Opaque i -> ( i, o )
"""
                    |> Review.Test.run
                        (fixInArgument
                            |> ifAsPatternRequired fail
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error """case o of
        Opaque i -> ( i, o )""" ]
        , test "to existing let" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

withUnpacked : Opaque -> ( Int, Opaque )
withUnpacked o =
    let
        foo =
            bar
    in
    case o of
        Opaque i -> ( i, o )
"""
                    |> Review.Test.run
                        (fixInArgument
                            |> ifAsPatternRequired
                                (fixInLetInstead
                                    |> andIfNoLetExists useAsPattern
                                )
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error """case o of
        Opaque i -> ( i, o )""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

withUnpacked : Opaque -> ( Int, Opaque )
withUnpacked o =
    let
        foo =
            bar

        (Opaque i) =
            o
    in
    ( i, o )
""" ]
        , test "no existing lets, back to as" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

withUnpacked : Opaque -> ( Int, Opaque )
withUnpacked o =
    case o of
        Opaque i -> ( i, o )
"""
                    |> Review.Test.run
                        (fixInArgument
                            |> ifAsPatternRequired
                                (fixInLetInstead
                                    |> andIfNoLetExists useAsPattern
                                )
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error """case o of
        Opaque i -> ( i, o )""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

withUnpacked : Opaque -> ( Int, Opaque )
withUnpacked ((Opaque i) as o) =
    ( i, o )
""" ]
        , test "no existing lets, fail" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

withUnpacked : Opaque -> ( Int, Opaque )
withUnpacked o =
    case o of
        Opaque i -> ( i, o )
"""
                    |> Review.Test.run
                        (fixInArgument
                            |> ifAsPatternRequired
                                (fixInLetInstead
                                    |> andIfNoLetExists fail
                                )
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error """case o of
        Opaque i -> ( i, o )""" ]
        , test "no existing lets, creates new" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

withUnpacked : Opaque -> ( Int, Opaque )
withUnpacked o =
    case o of
        Opaque i -> ( i, o )
"""
                    |> Review.Test.run
                        (fixInArgument
                            |> ifAsPatternRequired
                                (fixInLetInstead
                                    |> andIfNoLetExists createNewLet
                                )
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error """case o of
        Opaque i -> ( i, o )""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

withUnpacked : Opaque -> ( Int, Opaque )
withUnpacked o =
    let
        (Opaque i) =
            o
    in
    ( i, o )
""" ]
        ]


cannotDestructureInArgSuite : Test
cannotDestructureInArgSuite =
    describe "cannot destructure in argument"
        [ test "because case expression isn't just a variable" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    case ( o, o ) of
        ( Opaque i, Opaque ii ) -> i
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error """case ( o, o ) of
        ( Opaque i, Opaque ii ) -> i"""
                        ]
        , test "because case expression is variable out of scope" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

topLevel : Opaque
topLevel =
    Opaque 2

unpacked : Int
unpacked =
    case topLevel of
        Opaque i -> i
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error """case topLevel of
        Opaque i -> i"""
                        ]
        , test "because the variable in case expression is from a pattern after as" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack ((Opaque ii) as o) =
    case o of
        Opaque i -> i
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error """case o of
        Opaque i -> i"""
                        ]
        , test "because the variable in case expression is from a record field pattern" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : { o : Opaque } -> Int
unpack { o } =
    case o of
        Opaque i -> i
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error """case o of
        Opaque i -> i"""
                        ]
        , test "because the variable in case expression is an annotated let binding" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack oo =
    let
        o : Opaque
        o =
            oo

        unpacked =
            case o of
                Opaque i -> i
    in
    unpacked
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error """case o of
                Opaque i -> i"""
                        ]
        , test "because a var in increased scope would cause a name clash" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    let
        clash i =
            i
    in
    case o of
        Opaque i -> i
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error """case o of
        Opaque i -> i"""
                        ]
        , cannotDestructureFallbackSuite
        ]


cannotDestructureFallbackSuite : Test
cannotDestructureFallbackSuite =
    describe "falls back from unable to destructure"
        [ test "fails" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : { o : Opaque } -> Int
unpack { o } =
    case o of
        Opaque i -> i
"""
                    |> Review.Test.run
                        (fixInArgument
                            |> ifCannotDestructureAtArgument fail
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error """case o of
        Opaque i -> i""" ]
        , test "to existing let" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : { o : Opaque } -> Int
unpack { o } =
    let
        foo =
            bar
    in
    case o of
        Opaque i -> i
"""
                    |> Review.Test.run
                        (fixInArgument
                            |> ifCannotDestructureAtArgument
                                (fixInLetInstead
                                    |> andIfNoLetExists fail
                                )
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error """case o of
        Opaque i -> i""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : { o : Opaque } -> Int
unpack { o } =
    let
        foo =
            bar

        (Opaque i) =
            o
    in
    i
""" ]
        , test "no existing lets, fails" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : { o : Opaque } -> Int
unpack { o } =
    case o of
        Opaque i -> i
"""
                    |> Review.Test.run
                        (fixInArgument
                            |> ifCannotDestructureAtArgument
                                (fixInLetInstead
                                    |> andIfNoLetExists fail
                                )
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error """case o of
        Opaque i -> i""" ]
        , test "no existing lets, creates new" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : { o : Opaque } -> Int
unpack { o } =
    case o of
        Opaque i -> i
"""
                    |> Review.Test.run
                        (fixInArgument
                            |> ifCannotDestructureAtArgument
                                (fixInLetInstead
                                    |> andIfNoLetExists createNewLet
                                )
                            |> rule
                        )
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
""" ]
        ]


fixInLetSuite : Test
fixInLetSuite =
    describe "fixes in lets"
        [ test "simple case" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    let
        foo =
            bar
    in
    case o of
        Opaque i -> i
"""
                    |> Review.Test.run (rule fixInLet)
                    |> Review.Test.expectErrors
                        [ error """case o of
        Opaque i -> i""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    let
        foo =
            bar

        (Opaque i) =
            o
    in
    i
"""
                        ]
        , test "multiline expressions" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

withUnpacked : Opaque -> ( Int, List Int )
withUnpacked map =
    let
        foo =
            bar
    in
    case
        foo
            (always map)
    of
        Opaque i ->
            ( i
            , List.map ((+) 1) [ 1 ]
            )
"""
                    |> Review.Test.run (rule fixInLet)
                    |> Review.Test.expectErrors
                        [ error """case
        foo
            (always map)
    of
        Opaque i ->
            ( i
            , List.map ((+) 1) [ 1 ]
            )""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

withUnpacked : Opaque -> ( Int, List Int )
withUnpacked map =
    let
        foo =
            bar

        (Opaque i) =
            foo
                   (always map)
    in
    ( i
            , List.map ((+) 1) [ 1 ]
            )
"""
                        ]
        , test "expression contains binding multiple times" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    let
        foo =
            bar
    in
    case o of
        Opaque i -> i + i
"""
                    |> Review.Test.run (rule fixInLet)
                    |> Review.Test.expectErrors
                        [ error """case o of
        Opaque i -> i + i""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    let
        foo =
            bar

        (Opaque i) =
            o
    in
    i + i
"""
                        ]
        , test "in let declaration" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack =
    let
        f o =
            let
                foo =
                    bar
            in
            case foo
                |> o
            of
                Opaque i -> i
                    + i
    in
    f
"""
                    |> Review.Test.run (rule fixInLet)
                    |> Review.Test.expectErrors
                        [ error """case foo
                |> o
            of
                Opaque i -> i
                    + i""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack =
    let
        f o =
            let
                foo =
                    bar

                (Opaque i) =
                    foo
                       |> o
            in
            i
                    + i
    in
    f
"""
                        ]
        , test "in lambda" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack =
    \\o ->
        let
            foo =
                bar
        in
        case o of
            Opaque i -> i
"""
                    |> Review.Test.run (rule fixInLet)
                    |> Review.Test.expectErrors
                        [ error """case o of
            Opaque i -> i""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack =
    \\o ->
        let
            foo =
                bar

            (Opaque i) =
                o
        in
        i
"""
                        ]
        , test "in same let as case" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    let
        unpacked =
            case o of
                Opaque i -> i
    in
    unpacked
"""
                    |> Review.Test.run (rule fixInLet)
                    |> Review.Test.expectErrors
                        [ error """case o of
                Opaque i -> i""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    let
        unpacked =
            i

        (Opaque i) =
            o
    in
    unpacked
"""
                        ]
        , noExistingLetSuite
        ]


noExistingLetSuite : Test
noExistingLetSuite =
    describe "no suitable let exists"
        [ test "no block exists" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    case o of
        Opaque i -> i
"""
                    |> Review.Test.run
                        (fixInLet
                            |> ifNoLetExists fail
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error """case o of
        Opaque i -> i"""
                        ]
        , test "due to reliance on scope" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    let
        foo =
            bar
    in
    (\\a ->
    case a of
        Opaque i -> i
        ) o
"""
                    |> Review.Test.run
                        (fixInLet
                            |> ifNoLetExists fail
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error """case a of
        Opaque i -> i"""
                        ]
        , createNewLetSuite
        , fallbackToArgSuite
        ]


createNewLetSuite : Test
createNewLetSuite =
    describe "creates new let"
        [ test "simple" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    case o of
        Opaque i -> i
"""
                    |> Review.Test.run
                        (fixInLet
                            |> ifNoLetExists createNewLet
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error """case o of
        Opaque i -> i""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    let
        (Opaque i) =
            o
    in
    i
"""
                        ]
        , test "multiline expressions" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

withUnpacked : Opaque -> ( Int, List Int )
withUnpacked map =
    case
        foo
            (always map)
    of
        Opaque i ->
            ( i
            , List.map ((+) 1) [ 1 ]
            )
"""
                    |> Review.Test.run (rule fixInLet)
                    |> Review.Test.expectErrors
                        [ error """case
        foo
            (always map)
    of
        Opaque i ->
            ( i
            , List.map ((+) 1) [ 1 ]
            )""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

withUnpacked : Opaque -> ( Int, List Int )
withUnpacked map =
    let
        (Opaque i) =
            foo
                   (always map)
    in
    ( i
                , List.map ((+) 1) [ 1 ]
                )
"""
                        ]
        , test "complex scope" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    let
        foo =
            bar
    in
    (\\a ->
    case a of
        Opaque i -> i
        ) o
"""
                    |> Review.Test.run
                        (fixInLet
                            |> ifNoLetExists createNewLet
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error """case a of
        Opaque i -> i""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    let
        foo =
            bar
    in
    (\\a ->
    let
        (Opaque i) =
            a
    in
    i
        ) o
"""
                        ]
        ]


fallbackToArgSuite : Test
fallbackToArgSuite =
    describe "fallback to using argument"
        [ test "simple" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    case o of
        Opaque i -> i
"""
                    |> Review.Test.run
                        (fixInLet
                            |> ifNoLetExists
                                (fixInArgumentInstead
                                    |> andIfAsPatternRequired fail
                                    |> andIfCannotDestructureAtArgument fail
                                )
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error """case o of
        Opaque i -> i""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack (Opaque i) =
    i
"""
                        ]
        , test "complex scope" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    let
        foo =
            bar
    in
    (\\a ->
    case a of
        Opaque i -> i
        ) o
"""
                    |> Review.Test.run
                        (fixInLet
                            |> ifNoLetExists
                                (fixInArgumentInstead
                                    |> andIfAsPatternRequired fail
                                    |> andIfCannotDestructureAtArgument fail
                                )
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error """case a of
        Opaque i -> i""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    let
        foo =
            bar
    in
    (\\(Opaque i) ->
    i
        ) o
"""
                        ]
        , describe "if as required"
            [ test "uses as" <|
                \() ->
                    """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    case o of
        Opaque i -> ( o, i )
"""
                        |> Review.Test.run
                            (fixInLet
                                |> ifNoLetExists
                                    (fixInArgumentInstead
                                        |> andIfAsPatternRequired useAsPattern
                                        |> andIfCannotDestructureAtArgument fail
                                    )
                                |> rule
                            )
                        |> Review.Test.expectErrors
                            [ error """case o of
        Opaque i -> ( o, i )""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack ((Opaque i) as o) =
    ( o, i )
"""
                            ]
            , test "fails" <|
                \() ->
                    """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    case o of
        Opaque i -> ( o, i )
"""
                        |> Review.Test.run
                            (fixInLet
                                |> ifNoLetExists
                                    (fixInArgumentInstead
                                        |> andIfAsPatternRequired fail
                                        |> andIfCannotDestructureAtArgument fail
                                    )
                                |> rule
                            )
                        |> Review.Test.expectErrors
                            [ error """case o of
        Opaque i -> ( o, i )"""
                            ]
            , test "creates new let" <|
                \() ->
                    """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    case o of
        Opaque i -> ( o, i )
"""
                        |> Review.Test.run
                            (fixInLet
                                |> ifNoLetExists
                                    (fixInArgumentInstead
                                        |> andIfAsPatternRequired createNewLet
                                        |> andIfCannotDestructureAtArgument fail
                                    )
                                |> rule
                            )
                        |> Review.Test.expectErrors
                            [ error """case o of
        Opaque i -> ( o, i )""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    let
        (Opaque i) =
            o
    in
    ( o, i )
"""
                            ]
            ]
        , describe "if not destructurable"
            [ test "because case expression isn't just a variable" <|
                \() ->
                    """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    case ( o, o ) of
        ( Opaque i, Opaque ii ) -> i
"""
                        |> Review.Test.run
                            (fixInLet
                                |> ifNoLetExists
                                    (fixInArgumentInstead
                                        |> andIfAsPatternRequired createNewLet
                                        |> andIfCannotDestructureAtArgument fail
                                    )
                                |> rule
                            )
                        |> Review.Test.expectErrors
                            [ error """case ( o, o ) of
        ( Opaque i, Opaque ii ) -> i"""
                            ]
            , test "because case expression is variable out of scope" <|
                \() ->
                    """module A exposing (..)

type Opaque = Opaque Int

topLevel : Opaque
topLevel =
    Opaque 2

unpacked : Int
unpacked =
    case topLevel of
        Opaque i -> i
"""
                        |> Review.Test.run
                            (fixInLet
                                |> ifNoLetExists
                                    (fixInArgumentInstead
                                        |> andIfAsPatternRequired createNewLet
                                        |> andIfCannotDestructureAtArgument fail
                                    )
                                |> rule
                            )
                        |> Review.Test.expectErrors
                            [ error """case topLevel of
        Opaque i -> i"""
                            ]
            , test "because the variable in case expression is from a pattern after as" <|
                \() ->
                    """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack ((Opaque ii) as o) =
    case o of
        Opaque i -> i
"""
                        |> Review.Test.run
                            (fixInLet
                                |> ifNoLetExists
                                    (fixInArgumentInstead
                                        |> andIfAsPatternRequired createNewLet
                                        |> andIfCannotDestructureAtArgument fail
                                    )
                                |> rule
                            )
                        |> Review.Test.expectErrors
                            [ error """case o of
        Opaque i -> i"""
                            ]
            , test "because the variable in case expression is from a record field pattern" <|
                \() ->
                    """module A exposing (..)

type Opaque = Opaque Int

unpack : { o : Opaque } -> Int
unpack { o } =
    case o of
        Opaque i -> i
"""
                        |> Review.Test.run
                            (fixInLet
                                |> ifNoLetExists
                                    (fixInArgumentInstead
                                        |> andIfAsPatternRequired createNewLet
                                        |> andIfCannotDestructureAtArgument fail
                                    )
                                |> rule
                            )
                        |> Review.Test.expectErrors
                            [ error """case o of
        Opaque i -> i"""
                            ]
            , test "because a var in increased scope would cause a name clash" <|
                \() ->
                    """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    let
        clash i =
            i
    in
    case o of
        Opaque i -> i
"""
                        |> Review.Test.run
                            (fixInLet
                                |> ifNoLetExists
                                    (fixInArgumentInstead
                                        |> andIfAsPatternRequired createNewLet
                                        |> andIfCannotDestructureAtArgument fail
                                    )
                                |> rule
                            )
                        |> Review.Test.expectErrors
                            [ error """case o of
        Opaque i -> i"""
                            ]
            , describe "fallback"
                [ test "createNewLet" <|
                    \() ->
                        """module A exposing (..)

type Opaque = Opaque Int

unpack : { o : Opaque } -> Int
unpack { o } =
    case o of
        Opaque i -> i
"""
                            |> Review.Test.run
                                (fixInLet
                                    |> ifNoLetExists
                                        (fixInArgumentInstead
                                            |> andIfAsPatternRequired fail
                                            |> andIfCannotDestructureAtArgument createNewLet
                                        )
                                    |> rule
                                )
                            |> Review.Test.expectErrors
                                [ error """case o of
        Opaque i -> i"""
                                    |> Review.Test.whenFixed """module A exposing (..)

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
                ]
            ]
        ]


error : String -> Review.Test.ExpectedError
error under =
    Review.Test.error
        { message = "Single pattern case block."
        , details = [ "Single pattern case blocks typically are either unnecessary or overly verbose.  There's usually a more concise way to destructure, e.g. in a function argument, so consider refactoring." ]
        , under = under
        }
