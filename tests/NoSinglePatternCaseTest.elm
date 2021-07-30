module NoSinglePatternCaseTest exposing (all)

import NoSinglePatternCase exposing (alwaysFixInArgument, alwaysFixInLet, destructureInExistingLetsInstead, destructureTheArgument, destructureUsingAs, fixByDestructuringInExistingLets, fixByDestructuringTheArgument, noFix, noFixOnNameClash, rule)
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
                    |> Review.Test.run (rule alwaysFixInArgument)
                    |> Review.Test.expectNoErrors
        ]


disallowed : Test
disallowed =
    describe "does not allow"
        [ withUselessPattern
        , destructureTheArgumentTest
        , destructureInExistingLetsTest
        ]


withUselessPattern : Test
withUselessPattern =
    describe "useless pattern (_ or ())"
        [ describe "replace the argument"
            [ describe "possible"
                [ test "_" <|
                    \() ->
                        """module A exposing (..)

always2 : a -> Int
always2 a =
    case a of
        _ -> 2
"""
                            |> Review.Test.run (rule alwaysFixInArgument)
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
                            |> Review.Test.run (rule alwaysFixInArgument)
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
            ]
        , test "not possible" <|
            \() ->
                """module A exposing (..)

add2 : number -> number
add2 n =
    case n of
        _ -> n + 2
"""
                    |> Review.Test.run (rule alwaysFixInArgument)
                    |> Review.Test.expectErrors
                        [ error """case n of
        _ -> n + 2""" |> Review.Test.whenFixed """module A exposing (..)

add2 : number -> number
add2 n =
    n + 2
"""
                        ]
        , test "(don't) destructure in existing lets" <|
            \() ->
                """module A exposing (..)

always2 : a -> Int
always2 a =
    let
        foo =
            bar
    in
    case a of
        _ -> 2
"""
                    |> Review.Test.run (rule alwaysFixInLet)
                    |> Review.Test.expectErrors
                        [ error """case a of
        _ -> 2""" |> Review.Test.whenFixed """module A exposing (..)

always2 : a -> Int
always2 a =
    let
        foo =
            bar
    in
    2
"""
                        ]
        ]


destructureTheArgumentTest : Test
destructureTheArgumentTest =
    describe "destructure the argument"
        [ describe "possible"
            [ test "destructure named pattern" <|
                \() ->
                    """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    case o of
        Opaque i -> i
"""
                        |> Review.Test.run (rule alwaysFixInArgument)
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
                        |> Review.Test.run (rule alwaysFixInArgument)
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
        , describe "not possible"
            [ describe "because the variable in case and of is used multiple times"
                [ test "as used" <|
                    \() ->
                        """module A exposing (..)

type Opaque = Opaque Int

withUnpacked : Opaque -> ( Int, Opaque )
withUnpacked o =
    case o of
        Opaque i -> ( i, o )
"""
                            |> Review.Test.run (rule alwaysFixInArgument)
                            |> Review.Test.expectErrors
                                [ error """case o of
        Opaque i -> ( i, o )""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

withUnpacked : Opaque -> ( Int, Opaque )
withUnpacked ((Opaque i) as o) =
    ( i, o )
""" ]
                , describe "existing lets used"
                    [ test "possible" <|
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
                                    (rule
                                        (fixByDestructuringTheArgument
                                            { argumentAlsoUsedElsewhere =
                                                destructureInExistingLetsInstead
                                                    { noExistingLetsWhereArgumentIsntUsed =
                                                        destructureUsingAs
                                                    }
                                            , notDestructable = noFix
                                            }
                                            |> noFixOnNameClash
                                        )
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
                    , test "no existing lets, use as" <|
                        \() ->
                            """module A exposing (..)

type Opaque = Opaque Int

withUnpacked : Opaque -> ( Int, Opaque )
withUnpacked o =
    case o of
        Opaque i -> ( i, o )
"""
                                |> Review.Test.run
                                    (rule
                                        (fixByDestructuringTheArgument
                                            { argumentAlsoUsedElsewhere =
                                                destructureInExistingLetsInstead
                                                    { noExistingLetsWhereArgumentIsntUsed =
                                                        destructureUsingAs
                                                    }
                                            , notDestructable = noFix
                                            }
                                            |> noFixOnNameClash
                                        )
                                    )
                                |> Review.Test.expectErrors
                                    [ error """case o of
        Opaque i -> ( i, o )""" |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

withUnpacked : Opaque -> ( Int, Opaque )
withUnpacked ((Opaque i) as o) =
    ( i, o )
""" ]
                    ]
                ]
            , describe "can't use as"
                [ test "because in case and of isn't just a variable" <|
                    \() ->
                        """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    case ( o, o ) of
        ( Opaque i, Opaque ii ) -> i
"""
                            |> Review.Test.run (rule alwaysFixInArgument)
                            |> Review.Test.expectErrors
                                [ error """case ( o, o ) of
        ( Opaque i, Opaque ii ) -> i"""
                                ]
                , test "because the variable in case and of is defined outside of this function's scope" <|
                    \() ->
                        """module A exposing (..)

topLevel : Opaque
topLevel =
    Opaque 2

type Opaque = Opaque Int

unpacked : Int
unpacked =
    case topLevel of
        Opaque i -> i
"""
                            |> Review.Test.run (rule alwaysFixInArgument)
                            |> Review.Test.expectErrors
                                [ error """case topLevel of
        Opaque i -> i"""
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
                            |> Review.Test.run (rule alwaysFixInArgument)
                            |> Review.Test.expectErrors
                                [ error """case o of
        Opaque i -> i"""
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
                            |> Review.Test.run (rule alwaysFixInArgument)
                            |> Review.Test.expectErrors
                                [ error """case o of
        Opaque i -> i"""
                                ]
                , test "because the variable in case and of is the name of a let variable with an annotation" <|
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
                            |> Review.Test.run (rule alwaysFixInArgument)
                            |> Review.Test.expectErrors
                                [ error """case o of
                Opaque i -> i"""
                                ]
                , test "because a var pattern outside has the same name as in the case pattern → name clash" <|
                    \() ->
                        """module A exposing (..)

unpack : Opaque -> Int
unpack o =
    let
        something i =
            use i
    in
    case o of
        Opaque i -> i
"""
                            |> Review.Test.run (rule alwaysFixInArgument)
                            |> Review.Test.expectErrors
                                [ error """case o of
        Opaque i -> i"""
                                ]
                ]
            ]
        ]


destructureInExistingLetsTest : Test
destructureInExistingLetsTest =
    describe "in existing lets"
        [ test "possible" <|
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
                    |> Review.Test.run (rule alwaysFixInLet)
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
        , describe "no existing lets → destructureTheArgument"
            [ test "possible" <|
                \() ->
                    """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    case o of
        Opaque i -> i
"""
                        |> Review.Test.run
                            (rule
                                (fixByDestructuringInExistingLets
                                    { noExistingLets =
                                        destructureTheArgument
                                            { argumentAlsoUsedElsewhere = destructureUsingAs }
                                    }
                                    |> noFixOnNameClash
                                )
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
            , test "argument also used elsewhere, destructured using as" <|
                \() ->
                    """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    case o of
        Opaque i -> ( o, i )
"""
                        |> Review.Test.run
                            (rule
                                (fixByDestructuringInExistingLets
                                    { noExistingLets =
                                        destructureTheArgument
                                            { argumentAlsoUsedElsewhere =
                                                destructureUsingAs
                                            }
                                    }
                                    |> noFixOnNameClash
                                )
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
            , test "the argument is a record field pattern, no fix" <|
                \() ->
                    """module A exposing (..)

type Opaque = Opaque Int

unpack : { o : Opaque } -> Int
unpack { o } =
    case o of
        Opaque i -> i
"""
                        |> Review.Test.run
                            (rule
                                (fixByDestructuringInExistingLets
                                    { noExistingLets =
                                        destructureTheArgument
                                            { argumentAlsoUsedElsewhere =
                                                destructureUsingAs
                                            }
                                    }
                                    |> noFixOnNameClash
                                )
                            )
                        |> Review.Test.expectErrors
                            [ error """case o of
        Opaque i -> i"""
                            ]
            , test "between case and of is not just an argument, no fix" <|
                \() ->
                    """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    case ( o, o ) of
        ( Opaque i, Opaque ii ) -> i
"""
                        |> Review.Test.run
                            (rule
                                (fixByDestructuringInExistingLets
                                    { noExistingLets =
                                        destructureTheArgument
                                            { argumentAlsoUsedElsewhere =
                                                destructureUsingAs
                                            }
                                    }
                                    |> noFixOnNameClash
                                )
                            )
                        |> Review.Test.expectErrors
                            [ error """case ( o, o ) of
        ( Opaque i, Opaque ii ) -> i"""
                            ]
            ]
        , test "because a var pattern outside has the same name as in the case pattern → name clash" <|
            \() ->
                """module A exposing (..)

unpack : Opaque -> Int
unpack o =
    let
        something i =
            use i
    in
    case o of
        Opaque i -> i
"""
                    |> Review.Test.run
                        (rule
                            (fixByDestructuringInExistingLets
                                { noExistingLets = noFix }
                                |> noFixOnNameClash
                            )
                        )
                    |> Review.Test.expectErrors
                        [ error """case o of
        Opaque i -> i"""
                        ]
        ]


error : String -> Review.Test.ExpectedError
error under =
    Review.Test.error
        { message = "Single pattern case block."
        , details = [ "Single pattern case blocks are either unnecessary or overly verbose.  There's usually a more concise way to destructure, e.g. in a function argument, so consider refactoring." ]
        , under = under
        }
