module Util exposing
    ( Binding
    , Destructuring
    , Either(..)
    , bindingsInPattern
    , either3
    , nameClash
    , nameUsedOutsideExprs
    , namesUsedInExpression
    , reindent
    , subexpressions
    )

{-| General utility functions not directly related to the specific rule.
-}

import Dict exposing (Dict)
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
namesUsedInExpression : Node Expression -> Set String
namesUsedInExpression expr =
    case Node.value expr of
        -- If the name is qualified, it isn't a variable
        FunctionOrValue [] n ->
            Set.singleton n

        _ ->
            subexpressions expr
                -- Map and accumulate in one pass
                |> List.foldl (\e -> Set.union (namesUsedInExpression e)) Set.empty


{-| `Destructuring` represents a fully-reduced case expression.

  - `removableBindings` -- These are bindings that are used in removable parts of
    the `case...of` expression but nowhere else and can thus be safely replaced
    with `_`.
  - `usefulPatterns` -- These are actually necessery patterns (i.e. those that
    bind names used in the output expression), along with the expression they
    directly destructure.
  - `removedExpressions` -- These are expressions that will be removed when the
    `case` is rewritten, so we can ignore the use of anything in them.

-}
type alias Destructuring =
    { removableBindings : List { isUnit : Bool, binding : Binding }
    , usefulPatterns : List ( Node Pattern, Node Expression )
    , removedExpressions : List (Node Expression)
    }


{-| Combine two dictionaries. If there is a collision, a combining function is
used to combine the two values.

Note that, like `Dict.union`, it is more efficient to have the larger `Dict` as
the second argument, i.e. when possible, you should use `unionWith f new old`,
if `old` has more keys than `new`.

-}
dictUnionWith : (a -> a -> a) -> Dict comparable a -> Dict comparable a -> Dict comparable a
dictUnionWith f d1 d2 =
    Dict.foldl
        (\k v1 acc ->
            case Dict.get k acc of
                Just v2 ->
                    Dict.insert k (f v1 v2) acc

                Nothing ->
                    Dict.insert k v1 acc
        )
        d2
        d1


{-| Get a count for all names that are bound in a pattern, and the number of
times they are bound (which is always 1, but it's returned this way for ease of
combining with counts from expressions).
-}
nameAppearancesInPattern : Node Pattern -> Dict String Int
nameAppearancesInPattern p =
    -- Add pattern bindings, assuming names can't be bound more than once within a pattern (since they can't)
    namesInPattern p
        |> List.map (Tuple.mapSecond (always 1))
        |> Dict.fromList


{-| Given an expression, count every unqualified name that is bound or
referenced in it and the number of times it is bound and/or referenced.
-}
nameAppearancesInExpression : Node Expression -> Dict String Int
nameAppearancesInExpression expressionNode =
    let
        go : Node Expression -> Dict String Int
        go =
            nameAppearancesInExpression
    in
    case Node.value expressionNode of
        -- If the name is qualified, it isn't a variable
        FunctionOrValue [] n ->
            Dict.singleton n 1

        CaseExpression { cases, expression } ->
            List.map
                (\( p, e ) ->
                    nameAppearancesInPattern p
                        |> dictUnionWith (+) (go e)
                )
                cases
                |> List.foldl (dictUnionWith (+)) (go expression)

        LetExpression lB ->
            let
                bindingsFromDec : Node LetDeclaration -> Dict String Int
                bindingsFromDec d =
                    case Node.value d of
                        LetFunction fun ->
                            let
                                { name, expression, arguments } =
                                    Node.value fun.declaration
                            in
                            List.map nameAppearancesInPattern arguments
                                |> List.foldl (dictUnionWith (+)) (Dict.singleton (Node.value name) 1)
                                |> dictUnionWith (+) (go expression)

                        LetDestructuring pattern expr ->
                            nameAppearancesInPattern pattern
                                |> dictUnionWith (+) (go expr)
            in
            List.foldl (dictUnionWith (+) << bindingsFromDec) (go lB.expression) lB.declarations

        LambdaExpression { args, expression } ->
            List.map nameAppearancesInPattern args
                |> List.foldl (dictUnionWith (+)) (go expression)

        _ ->
            subexpressions expressionNode
                |> List.foldl (\e -> dictUnionWith (+) (go e)) Dict.empty


{-| Recursively find all names used in a pattern and save whether or not
destructuring could occur at the pattern. Requires scope information to actually
create the binding.
-}
namesInPattern : Node Pattern -> List ( String, Node Expression -> Binding )
namesInPattern pattern =
    let
        go : List (Node Pattern) -> List ( String, Node Expression -> Binding )
        go =
            List.concatMap namesInPattern

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
bindingsInPattern : Node Expression -> Node Pattern -> List ( String, Binding )
bindingsInPattern scope =
    namesInPattern
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


{-| Given some number of inner expression and an outer expression, and a list of
inner patterns, report all names bound in the patterns that are used in the
outer expression exclusive of the inner expressions or the patterns themselves.
-}
nameClash : { insideExpr : List (Node Expression), scope : Node Expression } -> List (Node Pattern) -> Bool
nameClash { insideExpr, scope } ps =
    let
        innerUses : Dict String Int
        innerUses =
            List.map nameAppearancesInExpression insideExpr
                |> List.foldl (dictUnionWith (+)) patternUses

        patternUses : Dict String Int
        patternUses =
            List.map nameAppearancesInPattern ps
                |> List.foldl (dictUnionWith (+)) Dict.empty

        outerUses : Dict String Int
        outerUses =
            nameAppearancesInExpression scope
    in
    DictX.any
        (\n _ ->
            Maybe.map2
                (\outerCount innerCount ->
                    outerCount > innerCount
                )
                (Dict.get n outerUses)
                (Dict.get n innerUses)
                |> Maybe.withDefault False
        )
        patternUses


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
