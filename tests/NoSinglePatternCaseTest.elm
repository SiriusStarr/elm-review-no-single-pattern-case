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
        [ test "using a case to destructure" <|
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
        Opaque i -> i"""
                        ]
        , test "using a case for no reason" <|
            \() ->
                """module A exposing (..)

type AOrB = A | B

pointless : AOrB -> Bool
pointless aOrB =
    case aOrB of
        _ -> True
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ error """case aOrB of
        _ -> True"""
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
