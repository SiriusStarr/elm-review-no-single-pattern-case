module Util exposing
    ( Binding
    , Either(..)
    , allBindingsInPattern
    , allBindingsUsedInExpression
    , allNamesInPattern
    , countUsesIn
    , either3
    , nameUsedOutsideExprs
    , reindent
    , subexpressions
    )

{-| General utility functions not directly related to the specific rule.
-}

import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Set exposing (Set)


{-| Get all immediate child expressions of an expression.
-}
subexpressions : Node Expression -> List (Node Expression)
subexpressions e =
    case Node.value e of
        LetExpression letBlock ->
            let
                subExprs : Node LetDeclaration -> Node Expression
                subExprs n =
                    case Node.value n of
                        LetFunction { declaration } ->
                            Node.value declaration
                                |> .expression

                        LetDestructuring _ expr ->
                            expr
            in
            letBlock.expression
                :: List.map subExprs letBlock.declarations

        ListExpr exprs ->
            exprs

        TupledExpression exprs ->
            exprs

        RecordExpr setters ->
            List.map (Tuple.second << Node.value) setters

        RecordUpdateExpression record updaters ->
            Node.map (FunctionOrValue []) record
                :: List.map (Tuple.second << Node.value) updaters

        Application exprs ->
            exprs

        CaseExpression caseBlock ->
            caseBlock.expression
                :: List.map Tuple.second caseBlock.cases

        OperatorApplication _ _ e1 e2 ->
            [ e1, e2 ]

        IfBlock predExpr thenExpr elseExpr ->
            [ predExpr, thenExpr, elseExpr ]

        LambdaExpression { expression } ->
            [ expression ]

        RecordAccess record _ ->
            [ record ]

        ParenthesizedExpression expr ->
            [ expr ]

        Negation expr ->
            [ expr ]

        UnitExpr ->
            []

        Integer _ ->
            []

        Hex _ ->
            []

        Floatable _ ->
            []

        Literal _ ->
            []

        CharLiteral _ ->
            []

        GLSLExpression _ ->
            []

        RecordAccessFunction _ ->
            []

        FunctionOrValue _ _ ->
            []

        Operator _ ->
            []

        PrefixOperator _ ->
            []


{-| Get a set of all (unqualified) bindings used in an expression.
-}
allBindingsUsedInExpression : Node Expression -> Set String
allBindingsUsedInExpression expr =
    case Node.value expr of
        -- If the name is qualified, it isn't a variable
        FunctionOrValue [] n ->
            Set.singleton n

        _ ->
            subexpressions expr
                -- Map and accumulate in one pass
                |> List.foldl (\e -> Set.union (allBindingsUsedInExpression <| e)) Set.empty


{-| Recursively find all names used in a pattern and save whether or not
destructuring could occur at the pattern. Requires scope information to actually
create the binding.
-}
allNamesInPattern : Node Pattern -> List ( String, Node Expression -> Binding )
allNamesInPattern pattern =
    let
        go : List (Node Pattern) -> List ( String, Node Expression -> Binding )
        go =
            List.concatMap allNamesInPattern

        makeBinding : Bool -> Node String -> ( String, Node Expression -> Binding )
        makeBinding canDestructureAt name =
            ( Node.value name
            , \scope ->
                { patternNodeRange = Node.range name
                , canDestructureAt = canDestructureAt
                , scope = scope
                }
            )
    in
    case Node.value pattern of
        ListPattern ps ->
            go ps

        TuplePattern ps ->
            go ps

        RecordPattern ps ->
            List.map (makeBinding False) ps

        NamedPattern _ ps ->
            go ps

        UnConsPattern p ps ->
            go [ p, ps ]

        VarPattern name ->
            [ ( name
              , \scope ->
                    { patternNodeRange = Node.range pattern
                    , canDestructureAt = True
                    , scope = scope
                    }
              )
            ]

        AsPattern p name ->
            makeBinding False name :: go [ p ]

        ParenthesizedPattern p ->
            go [ p ]

        AllPattern ->
            []

        UnitPattern ->
            []

        CharPattern _ ->
            []

        StringPattern _ ->
            []

        IntPattern _ ->
            []

        HexPattern _ ->
            []

        FloatPattern _ ->
            []


{-| Recursively find all bindings in a pattern and save whether or not
destructuring could occur at the pattern.
-}
allBindingsInPattern : Node Expression -> Node Pattern -> List ( String, Binding )
allBindingsInPattern scope =
    allNamesInPattern
        >> List.map (Tuple.mapSecond (\f -> f scope))


{-| Count the uses of a given name in the scope of an expression.
-}
countUsesIn : Node Expression -> String -> Int
countUsesIn expr name =
    case Node.value expr of
        -- If the name is qualified, it isn't a variable
        FunctionOrValue [] n ->
            if n == name then
                1

            else
                0

        _ ->
            subexpressions expr
                -- Count and sum in one pass
                |> List.foldl (\e -> (+) (countUsesIn e name)) 0


{-| Given a name, some number of inner expression, and an outer expression,
report if the name is used in the outer expression exclusive of the inner
expressions.
-}
nameUsedOutsideExprs : String -> { inside : List (Node Expression), scope : Node Expression } -> Bool
nameUsedOutsideExprs name { inside, scope } =
    countUsesIn scope name > List.foldl (\e acc -> acc + countUsesIn e name) 0 inside


{-| A binding with some scope.

  - `patternNodeRange` -- The `Range` for the binding location.
  - `canDestructureAt` -- Whether or not the binding can be destructured at.
  - `scope` -- The scope the binding is bound within.

-}
type alias Binding =
    { patternNodeRange : Range
    , canDestructureAt : Bool
    , scope : Node Expression
    }


{-| Re-indent a section of generated code to ensure that it doesn't cause issues
when used as a fix.
-}
reindent : Int -> String -> String
reindent amount =
    let
        indent : String
        indent =
            String.repeat (amount - 1) " "
    in
    String.lines
        >> List.map
            (\l ->
                -- Don't indent empty lines
                if String.isEmpty l then
                    l

                else
                    indent ++ l
            )
        >> String.join "\n"
        >> String.trimLeft


{-| Allow a choice between two options.
-}
type Either a b
    = A a
    | B b


{-| Case analysis for the `Either` type.
-}
either : (a -> c) -> (b -> c) -> Either a b -> c
either fA fB aOrB =
    case aOrB of
        A a ->
            fA a

        B b ->
            fB b


{-| Case analysis for nested `Either` type.
-}
either3 : Either (Either a b) c -> (a -> out) -> (b -> out) -> (c -> out) -> out
either3 abc a b c =
    either (either a b) c abc
