module NoSinglePatternCaseTest exposing (all)

import Dependencies.Dependency
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
        , replaceUnusedBindingsWithWildcard
        , reportAllCustomTypes
        , rule
        , useAsPattern
        )
import Review.Project exposing (addDependency)
import Review.Test
import Review.Test.Dependencies exposing (projectWithElmCore)
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
        , nonOpaqueSuite
        ]


disallowed : Test
disallowed =
    describe "does not allow"
        [ uselessPattern
        , fixInArgSuite
        , fixInLetSuite
        ]


nonOpaqueSuite : Test
nonOpaqueSuite =
    describe "non-opaque types"
        [ test "does not flag by default" <|
            \() ->
                """module A exposing (..)

type Msg = ThingieClicked

update : Msg -> Int -> Int
update msg i =
    case msg of
        ThingieClicked ->
            i + 1
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectNoErrors
        , test "does flag opaque" <|
            \() ->
                """module A exposing (..)

type Date = Date Int

update : Date -> Int -> Int
update date i =
    case date of
        Date j ->
            i + j
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error "Date j"
                            |> Review.Test.whenFixed """module A exposing (..)

type Date = Date Int

update : Date -> Int -> Int
update (Date j) i =
    i + j
"""
                        ]
        , test "does flag type from other module" <|
            \() ->
                [ """module A exposing (..)

type Date = Date Int
"""
                , """module B exposing (..)

import A

update : A.Date -> Int -> Int
update date i =
    case date of
        A.Date j ->
            i + j
"""
                ]
                    |> Review.Test.runOnModules
                        (rule fixInArgument)
                    |> Review.Test.expectErrorsForModules
                        [ ( "B"
                          , [ error "A.Date j"
                                |> Review.Test.whenFixed """module B exposing (..)

import A

update : A.Date -> Int -> Int
update (A.Date j) i =
    i + j
"""
                            ]
                          )
                        ]
        , test "does not flag non-opaque type from other module" <|
            \() ->
                [ """module A exposing (Msg(..))

type Msg = ThingClicked
"""
                , """module B exposing (..)

import A

update : A.Msg -> Int -> Int
update msg i =
    case msg of
        A.ThingClicked ->
            i + 1
"""
                ]
                    |> Review.Test.runOnModules
                        (rule fixInArgument)
                    |> Review.Test.expectNoErrors
        , test "does flag non-opaque type from other module that is not exported (this just tests context, since it's a compile error)" <|
            \() ->
                [ """module A exposing ()

type Msg = ThingClicked
"""
                , """module B exposing (..)

import A

update : A.Msg -> Int -> Int
update msg i =
    case msg of
        A.ThingClicked ->
            i + 1
"""
                ]
                    |> Review.Test.runOnModules
                        (rule fixInArgument)
                    |> Review.Test.expectErrorsForModules
                        [ ( "B"
                          , [ error "A.ThingClicked"
                                |> Review.Test.whenFixed """module B exposing (..)

import A

update : A.Msg -> Int -> Int
update msg i =
    i + 1
"""
                            ]
                          )
                        ]
        , test "does not flag non-opaque type from same module that is not exported" <|
            \() ->
                """module B exposing (update)

import A

type Msg = ThingClicked

update : A.Msg -> Int -> Int
update msg i =
    case msg of
        ThingClicked ->
            i + 1
"""
                    |> Review.Test.run
                        (rule fixInArgument)
                    |> Review.Test.expectNoErrors
        , test "does flag wrapped type from dependency" <|
            \() ->
                """module A exposing (..)

import Dependency exposing (Wrapped(..))

update : Wrapped -> Int -> Int
update wrapped i =
    case wrapped of
        Wrapped j ->
            i + j
"""
                    |> Review.Test.runWithProjectData
                        (projectWithElmCore
                            |> addDependency Dependencies.Dependency.dependency
                        )
                        (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error "Wrapped j"
                            |> Review.Test.whenFixed """module A exposing (..)

import Dependency exposing (Wrapped(..))

update : Wrapped -> Int -> Int
update (Wrapped j) i =
    i + j
"""
                        ]
        , test "does flag non-wrapped type from dependency" <|
            \() ->
                """module A exposing (..)

import Dependency exposing (Msg(..))

update : Msg -> Int -> Int
update msg i =
    case msg of
        ThingClicked ->
            i + 1
"""
                    |> Review.Test.runWithProjectData
                        (projectWithElmCore
                            |> addDependency Dependencies.Dependency.dependency
                        )
                        (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error "ThingClicked"
                            |> Review.Test.whenFixed """module A exposing (..)

import Dependency exposing (Msg(..))

update : Msg -> Int -> Int
update msg i =
    i + 1
"""
                        ]
        , test "name must be exact" <|
            \() ->
                """module A exposing (..)

type Msg1 = WrappedMsg1

type Msg2 = Msg2Wrapped

update : Msg1 -> Msg2 -> Int
update msg1 msg2 i =
    case (msg1, msg2) of
        (WrappedMsg1, Msg2Wrapped) ->
            0
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectNoErrors
        , test "reduces mixed cases" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

type Msg1 = WrappedMsg1 Int

type Msg2 = Msg2Wrapped Int

foo : Msg1 -> Opaque -> Msg2 -> Int
foo a b c =
    case (a, b, c) of
        (WrappedMsg1 i, Opaque j, Msg2Wrapped k) -> i + j + k
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error "(WrappedMsg1 i, Opaque j, Msg2Wrapped k)"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

type Msg1 = WrappedMsg1 Int

type Msg2 = Msg2Wrapped Int

foo : Msg1 -> Opaque -> Msg2 -> Int
foo a (Opaque j) c =
    case (a, c) of
        (WrappedMsg1 i, Msg2Wrapped k) ->
            i + j + k
"""
                        ]
        , test "reduces mixed cases multiline" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

type Msg1 = WrappedMsg1 Int

type Msg2 = Msg2Wrapped Int

foo : Msg1 -> Opaque -> Msg2 -> Int
foo a b c =
    case
        ( a
            |> foo
            |> bar
        , b
        , c
        )
    of
        ( WrappedMsg1 i, Opaque j, Msg2Wrapped k ) ->
            i
                + j
                + k
                |> foo
                |> bar
                |> baz
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error "( WrappedMsg1 i, Opaque j, Msg2Wrapped k )"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

type Msg1 = WrappedMsg1 Int

type Msg2 = Msg2Wrapped Int

foo : Msg1 -> Opaque -> Msg2 -> Int
foo a (Opaque j) c =
    case (a
                |> foo
                |> bar, c) of
        (WrappedMsg1 i, Msg2Wrapped k) ->
            i
                    + j
                    + k
                    |> foo
                    |> bar
                    |> baz
"""
                        ]
        , test "flags with setting" <|
            \() ->
                """module A exposing (..)

type Msg = ThingieClicked

update : Msg -> Int -> Int
update msg i =
    case msg of
        ThingieClicked ->
            i + 1
"""
                    |> Review.Test.run
                        (fixInArgument
                            |> reportAllCustomTypes
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error "ThingieClicked"
                            |> Review.Test.atExactly { start = { row = 8, column = 9 }, end = { row = 8, column = 23 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type Msg = ThingieClicked

update : Msg -> Int -> Int
update msg i =
    i + 1
"""
                        ]
        , test "reduces all with setting" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

type Msg1 = WrappedMsg1 Int

type Msg2 = Msg2Wrapped Int

foo : Msg1 -> Opaque -> Msg2 -> Int
foo a b c =
    case (a, b, c) of
        (WrappedMsg1 i, Opaque j, Msg2Wrapped k) -> i + j + k
"""
                    |> Review.Test.run
                        (fixInArgument
                            |> reportAllCustomTypes
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error "(WrappedMsg1 i, Opaque j, Msg2Wrapped k)"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

type Msg1 = WrappedMsg1 Int

type Msg2 = Msg2Wrapped Int

foo : Msg1 -> Opaque -> Msg2 -> Int
foo (WrappedMsg1 i) (Opaque j) (Msg2Wrapped k) =
    i + j + k
"""
                        ]
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
                        [ error "_"
                            |> Review.Test.whenFixed """module A exposing (..)

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
                        [ error "()"
                            |> Review.Test.atExactly { start = { row = 6, column = 9 }, end = { row = 6, column = 11 } }
                            |> Review.Test.whenFixed """module A exposing (..)

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
                        [ error "({ n } as r)"
                            |> Review.Test.whenFixed """module A exposing (..)

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
                        [ error "A int"
                            |> Review.Test.whenFixed """module A exposing (..)

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
                        [ error "_"
                            |> Review.Test.whenFixed """module A exposing (..)

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
                        [ error "()"
                            |> Review.Test.atExactly { start = { row = 6, column = 9 }, end = { row = 6, column = 11 } }
                            |> Review.Test.whenFixed """module A exposing (..)

pointless : () -> Bool
pointless () =
    True
"""
                        ]
        , test "constructor no args" <|
            \() ->
                """module A exposing (..)

type CreateNewLet = CreateNewLet

pointless : CreateNewLet -> Bool
pointless a =
    case a of
        CreateNewLet ->
            foo
"""
                    |> Review.Test.run
                        (fixInArgument
                            |> replaceUnusedBindings
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error "CreateNewLet"
                            |> Review.Test.atExactly { start = { row = 8, column = 9 }, end = { row = 8, column = 21 } }
                            |> Review.Test.whenFixed """module A exposing (..)

type CreateNewLet = CreateNewLet

pointless : CreateNewLet -> Bool
pointless CreateNewLet =
    foo
"""
                        ]
        , test "type from other module" <|
            \() ->
                [ """module A exposing (..)

type Date = Date Int
"""
                , """module B exposing (..)

import A

update : A.Date -> Int -> Int
update date i =
    case date of
        A.Date _ ->
            i
"""
                ]
                    |> Review.Test.runOnModules
                        (fixInArgument
                            |> replaceUnusedBindings
                            |> rule
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "B"
                          , [ error "A.Date _"
                                |> Review.Test.whenFixed """module B exposing (..)

import A

update : A.Date -> Int -> Int
update (A.Date _) i =
    i
"""
                            ]
                          )
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
                        [ error "({ n } as r)"
                            |> Review.Test.whenFixed """module A exposing (..)

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
                        [ error "A int"
                            |> Review.Test.whenFixed """module A exposing (..)

type A = A Int

pointless : A -> Bool
pointless (A _) =
    True
"""
                        ]
        , test "complex case" <|
            \() ->
                """module A exposing (..)

type Date = Date Int

type CreateThing = CreateThing

foo : ( Date, CreateThing ) -> Bool
foo x =
    case x of
        ( Date i, CreateThing ) ->
            True
"""
                    |> Review.Test.run
                        (fixInArgument
                            |> replaceUnusedBindings
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error "( Date i, CreateThing )"
                            |> Review.Test.whenFixed """module A exposing (..)

type Date = Date Int

type CreateThing = CreateThing

foo : ( Date, CreateThing ) -> Bool
foo ( (Date _), CreateThing ) =
    True
"""
                        ]
        , test "tuple no sub bindings" <|
            \() ->
                """module A exposing (..)

pointless : (Int, String) -> Bool
pointless a =
    case a of
        (x, y) -> True
"""
                    |> Review.Test.run
                        (fixInArgument
                            |> replaceUnusedBindings
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error "(x, y)"
                            |> Review.Test.whenFixed """module A exposing (..)

pointless : (Int, String) -> Bool
pointless _ =
    True
"""
                        ]
        , test "tuple with sub bindings" <|
            \() ->
                """module A exposing (..)

type A = A Int

pointless : (Int, A) -> Bool
pointless a =
    case a of
        (x, A i) -> True
"""
                    |> Review.Test.run
                        (fixInArgument
                            |> replaceUnusedBindings
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error "(x, A i)"
                            |> Review.Test.whenFixed """module A exposing (..)

type A = A Int

pointless : (Int, A) -> Bool
pointless ( _, (A _) ) =
    True
"""
                        ]
        , test "single custom type with wildcard option" <|
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
                            |> replaceUnusedBindingsWithWildcard
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error "A int"
                            |> Review.Test.whenFixed """module A exposing (..)

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
                        [ error "B (A i1) (A i2)"
                            |> Review.Test.whenFixed """module A exposing (..)

type A = A Int

type B = B A A

pointless : B -> Bool
pointless (B (A _) (A _)) =
    True
"""
                        ]
        , test "nested custom types with wildcard option" <|
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
                            |> replaceUnusedBindingsWithWildcard
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error "B (A i1) (A i2)"
                            |> Review.Test.whenFixed """module A exposing (..)

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
                        [ error "(_, _)"
                            |> Review.Test.whenFixed """module A exposing (..)

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
                        [ error "A int"
                            |> Review.Test.whenFixed """module A exposing (..)

type A = A Int

pointless : A -> Bool
pointless a =
    let
        (A _) = a
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
                        [ error "(_, _, _)"
                            |> Review.Test.whenFixed """module A exposing (..)

pointless : Int -> Int -> Int -> Bool
pointless _ b _ =
    let
        foo = b
    in
    True
"""
                        ]
        , test "multiple some used" <|
            \() ->
                """module A exposing (..)

pointless : Int -> Int -> () -> Int
pointless a b c =
    case (a, b, c) of
        (_, b_, ()) -> b_
"""
                    |> Review.Test.run
                        (fixInArgument
                            |> replaceUnusedBindings
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error "(_, b_, ())"
                            |> Review.Test.whenFixed """module A exposing (..)

pointless : Int -> Int -> () -> Int
pointless _ (b_) () =
    b_
"""
                        ]
        , test "multiple in record some used" <|
            \() ->
                """module A exposing (..)

pointless : Int -> Int -> () -> Int
pointless a b c =
    case { d = a, e = b, f = c } of
        { d, e, f } -> e
"""
                    |> Review.Test.run
                        (fixInArgument
                            |> replaceUnusedBindings
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error "{ d, e, f }"
                            |> Review.Test.whenFixed """module A exposing (..)

pointless : Int -> Int -> () -> Int
pointless _ (e) _ =
    e
"""
                        ]
        , test "multiple in record some used repeatedly" <|
            \() ->
                """module A exposing (..)

pointless : Int -> Int -> Int -> Int
pointless a b c =
    case { d = a, e = b, f = c, g = a, h = b, i = c } of
        { d, e, f } -> e
"""
                    |> Review.Test.run
                        (fixInArgument
                            |> replaceUnusedBindings
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error "{ d, e, f }"
                            |> Review.Test.whenFixed """module A exposing (..)

pointless : Int -> Int -> Int -> Int
pointless _ (e) _ =
    e
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
                        [ error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

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
                        [ error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

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
                        [ error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

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
                        [ error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack =
    \\(Opaque i) ->
        i
"""
                        ]
        , asPatternSuite
        , cannotDestructureInArgSuite
        , nestedSuite
        , getBindingSuite
        ]


getBindingSuite : Test
getBindingSuite =
    describe "get bindings"
        [ test "parenthesized" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    case (o) of
        Opaque i -> i
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack (Opaque i) =
    i
"""
                        ]
        , test "function app" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

type Tuple a b = Tuple a b

weirdWrap : Int -> Int -> Opaque Int
weirdWrap a b =
    case (Tuple a b) of
        Tuple i1 i2 ->
            Opaque <| i1 * i2
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error "Tuple i1 i2"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

type Tuple a b = Tuple a b

weirdWrap : Int -> Int -> Opaque Int
weirdWrap (i1) (i2) =
    Opaque <| i1 * i2
"""
                        ]
        , test "tupled" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

add : Opaque -> Opaque -> Opaque
add a b =
    case ( a, b ) of
        ( Opaque i1, Opaque i2 ) ->
            Opaque <| i1 + i2
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error "( Opaque i1, Opaque i2 )"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

add : Opaque -> Opaque -> Opaque
add (Opaque i1) (Opaque i2) =
    Opaque <| i1 + i2
"""
                        ]
        , test "tupled same binding" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    case ( o, o ) of
        ( Opaque i, Opaque ii ) -> i + ii
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error "( Opaque i, Opaque ii )"
                        ]
        , test "tupled to let" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

add : Opaque -> Opaque -> Opaque
add a b =
    case ( a, b ) of
        ( Opaque i1, Opaque i2 ) ->
            Opaque <| i1 + i2
"""
                    |> Review.Test.run (rule fixInLet)
                    |> Review.Test.expectErrors
                        [ error "( Opaque i1, Opaque i2 )"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

add : Opaque -> Opaque -> Opaque
add a b =
    let
        (Opaque i1) =
            a

        (Opaque i2) =
            b
    in
    Opaque <| i1 + i2
"""
                        ]
        , test "tupled both as needed" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

add : Opaque -> Opaque -> (Opaque, Opaque, Opaque)
add a b =
    case ( a, b ) of
        ( Opaque i1, Opaque i2 ) ->
            (Opaque <| i1 + i2, a, b)
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error "( Opaque i1, Opaque i2 )"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

add : Opaque -> Opaque -> (Opaque, Opaque, Opaque)
add ((Opaque i1) as a) ((Opaque i2) as b) =
    (Opaque <| i1 + i2, a, b)
"""
                        ]
        , test "tupled one as needed" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

add : Opaque -> Opaque -> (Opaque, Opaque)
add a b =
    case ( a, b ) of
        ( Opaque i1, Opaque i2 ) ->
            (Opaque <| i1 + i2, a)
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error "( Opaque i1, Opaque i2 )"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

add : Opaque -> Opaque -> (Opaque, Opaque)
add ((Opaque i1) as a) (Opaque i2) =
    (Opaque <| i1 + i2, a)
"""
                        ]
        , test "tupled one as needed fallback to let" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

add : Opaque -> Opaque -> (Opaque, Opaque)
add a b =
    case ( a, b ) of
        ( Opaque i1, Opaque i2 ) ->
            (Opaque <| i1 + i2, a)
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
                        [ error "( Opaque i1, Opaque i2 )"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

add : Opaque -> Opaque -> (Opaque, Opaque)
add a (Opaque i2) =
    let
        (Opaque i1) =
            a
    in
    (Opaque <| i1 + i2, a)
"""
                        ]
        , test "tupled one as needed use let instead" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

add : Opaque -> Opaque -> (Opaque, Opaque)
add a b =
    case ( a, b ) of
        ( Opaque i1, Opaque i2 ) ->
            (Opaque <| i1 + i2, a)
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
                        [ error "( Opaque i1, Opaque i2 )"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

add : Opaque -> Opaque -> (Opaque, Opaque)
add a (Opaque i2) =
    let
        (Opaque i1) =
            a
    in
    (Opaque <| i1 + i2, a)
"""
                        ]
        , test "tupled one destructurable use let instead" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

add : { a : Opaque } -> Opaque -> Opaque
add {a} b =
    case ( a, b ) of
        ( Opaque i1, Opaque i2 ) ->
            Opaque <| i1 + i2
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
                        [ error "( Opaque i1, Opaque i2 )"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

add : { a : Opaque } -> Opaque -> Opaque
add {a} (Opaque i2) =
    let
        (Opaque i1) =
            a
    in
    Opaque <| i1 + i2
"""
                        ]
        , test "as pattern" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> (Int, Opaque)
unpack o =
    case o of
        ((Opaque i) as a) -> (i, a)
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error "((Opaque i) as a)"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> (Int, Opaque)
unpack ((Opaque i) as a) =
    (i, a)
"""
                        ]
        , test "unnecessary as pattern" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> (Int, Opaque)
unpack o =
    case o of
        ((Opaque i) as a) -> i
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error "((Opaque i) as a)"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> (Int, Opaque)
unpack (Opaque i) =
    i
"""
                        ]
        , test "unnecessary tupled as pattern" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

add : Opaque -> Opaque -> Opaque
add a b =
    case ( a, b ) of
        (( Opaque i1, Opaque i2 ) as unn) ->
            Opaque <| i1 + i2
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error "(( Opaque i1, Opaque i2 ) as unn)"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

add : Opaque -> Opaque -> Opaque
add (Opaque i1) (Opaque i2) =
    Opaque <| i1 + i2
"""
                        ]
        , test "necessary tupled as pattern" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

add : Opaque -> Opaque -> (Opaque, Opaque)
add a b =
    case ( a, b ) of
        (( Opaque i1, Opaque i2 ) as x) ->
            (Opaque <| i1 + i2, x)
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
                        [ error "(( Opaque i1, Opaque i2 ) as x)"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

add : Opaque -> Opaque -> (Opaque, Opaque)
add a b =
    let
        (( Opaque i1, Opaque i2 ) as x) =
            ( a, b )
    in
    (Opaque <| i1 + i2, x)
"""
                        ]
        , test "conflicting as pattern" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> (Int, Opaque, Opaque)
unpack o =
    case o of
        ((Opaque i) as a) -> (i, a, o)
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error "((Opaque i) as a)"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> (Int, Opaque, Opaque)
unpack (((Opaque i) as a) as o) =
    (i, a, o)
"""
                        ]
        ]


nestedSuite : Test
nestedSuite =
    describe "nested single pattern"
        [ test "simple case" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    case () of
        _ ->
            case o of
                Opaque i -> i
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error "_"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    case o of
                Opaque i -> i
"""
                        , error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack (Opaque i) =
    case () of
        _ ->
            i
"""
                        ]
        , test "nested same" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    case o of
        Opaque i ->
            case o of
                Opaque j -> j
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    case o of
                Opaque j -> j
"""
                        , error "Opaque j"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack ((Opaque j) as o) =
    case o of
        Opaque i ->
            j
"""
                        ]
        , test "nested in case ... of" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    case
        case o of
            Opaque i ->
                i
    of
        1 ->
            True
        _ ->
            False
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack (Opaque i) =
    case
        i
    of
        1 ->
            True
        _ ->
            False
"""
                        ]
        , test "nested in single-pattern case ... of" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    case
        case o of
            Opaque i ->
                i
    of
        _ ->
            True
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error "_"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    True
"""
                        , error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack (Opaque i) =
    case
        i
    of
        _ ->
            True
"""
                        ]
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
                        [ error "Opaque i"
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
                        [ error "Opaque i"
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
                        [ error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

withUnpacked : Opaque -> ( Int, Opaque )
withUnpacked ((Opaque i) as o) =
    ( i, o )
"""
                        ]
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
                        [ error "Opaque i"
                            |> Review.Test.atExactly { start = { row = 12, column = 17 }, end = { row = 12, column = 25 } }
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
                        , error "Opaque i"
                            |> Review.Test.atExactly { start = { row = 15, column = 17 }, end = { row = 15, column = 25 } }
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
                        [ error "Opaque i"
                            |> Review.Test.atExactly { start = { row = 13, column = 17 }, end = { row = 13, column = 25 } }
                            |> Review.Test.whenFixed """module A exposing (..)

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
                        [ error "Opaque i" ]
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
                        [ error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

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
"""
                        ]
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
                        [ error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

withUnpacked : Opaque -> ( Int, Opaque )
withUnpacked ((Opaque i) as o) =
    ( i, o )
"""
                        ]
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
                        [ error "Opaque i" ]
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
                        [ error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

withUnpacked : Opaque -> ( Int, Opaque )
withUnpacked o =
    let
        (Opaque i) =
            o
    in
    ( i, o )
"""
                        ]
        ]


cannotDestructureInArgSuite : Test
cannotDestructureInArgSuite =
    describe "cannot destructure in argument"
        [ test "because case expression isn't just a variable" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : { a : Opaque } -> Int
unpack a =
    case a.o of
        Opaque i -> i
"""
                    |> Review.Test.run (rule fixInArgument)
                    |> Review.Test.expectErrors
                        [ error "Opaque i"
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
                        [ error "Opaque i"
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
                        [ error "Opaque i"
                            |> Review.Test.atExactly { start = { row = 8, column = 9 }, end = { row = 8, column = 17 } }
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
                        [ error "Opaque i"
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
                        [ error "Opaque i"
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
                        [ error "Opaque i"
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
                        [ error "Opaque i" ]
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
                        [ error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

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
"""
                        ]
        , test "fall back due to name clash clash in pattern" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    let
        foo i =
            0
    in
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
                        [ error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    let
        foo i =
            0
    in
    let
        (Opaque i) =
            o
    in
    i
"""
                        ]
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
                        [ error "Opaque i" ]
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
                        [ error "Opaque i"
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
                        [ error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

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
                        [ error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

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
                        [ error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

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
                        [ error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

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
                        [ error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

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
                        [ error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

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
                        [ error "Opaque i"
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
                        [ error "Opaque i"
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
                        [ error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

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
                        [ error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

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
                        [ error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

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
        , test "name clash in pattern" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    let
        foo i =
            0
    in
    case o of
        Opaque i -> i
"""
                    |> Review.Test.run
                        (fixInLet
                            |> ifNoLetExists createNewLet
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    let
        foo i =
            0
    in
    let
        (Opaque i) =
            o
    in
    i
"""
                        ]
        , test "name clash but removable" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    let
        foo a =
            0
    in
    case (o, o) of
        (Opaque i, a) -> i
"""
                    |> Review.Test.run
                        (fixInLet
                            |> ifNoLetExists createNewLet
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error "(Opaque i, a)"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Int
unpack o =
    let
        foo a =
            0

        (Opaque i) =
            o
    in
    i
"""
                        ]
        , test "name clash in one, not other" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> (Int, Opaque)
unpack o =
    let
        foo a =
            0
    in
    case (o, o) of
        (Opaque i, a) -> (i, a)
"""
                    |> Review.Test.run
                        (fixInLet
                            |> ifNoLetExists createNewLet
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error "(Opaque i, a)"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> (Int, Opaque)
unpack o =
    let
        foo a =
            0

        (Opaque i) =
            o
    in
    let
        (a) =
            o
    in
    (i, a)
"""
                        ]
        , test "not in scope for one, not other" <|
            \() ->
                """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Opaque -> (Int, Int)
unpack o =
    let
        foo = bar
    in
    (\\x ->
        case (o, x) of
            (Opaque i, Opaque j) -> (i, j)
    )
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
                        [ error "(Opaque i, Opaque j)"
                            |> Review.Test.whenFixed """module A exposing (..)

type Opaque = Opaque Int

unpack : Opaque -> Opaque -> (Int, Int)
unpack o =
    let
        foo = bar

        (Opaque i) =
            o
    in
    (\\(Opaque j) ->
        (i, j)
    )
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
                        [ error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

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
                        [ error "Opaque i"
                            |> Review.Test.whenFixed """module A exposing (..)

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
                            [ error "Opaque i"
                                |> Review.Test.whenFixed """module A exposing (..)

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
                            [ error "Opaque i"
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
                            [ error "Opaque i"
                                |> Review.Test.whenFixed """module A exposing (..)

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

unpack : { a : Opaque } -> Int
unpack a =
    case a.o of
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
                            [ error "Opaque i"
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
                            [ error "Opaque i"
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
                            [ error "Opaque i"
                                |> Review.Test.atExactly { start = { row = 8, column = 9 }, end = { row = 8, column = 17 } }
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
                            [ error "Opaque i"
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
                            [ error "Opaque i"
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
                                [ error "Opaque i"
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
