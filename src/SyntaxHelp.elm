module SyntaxHelp exposing (VarPatternKind(..), allVarsInPattern, expressionsInExpression, parensAroundNamedPattern, usesIn)

import Elm.CodeGen exposing (parensPattern, val)
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))


{-| The direct child expressions.
-}
expressionsInExpression : Expression -> List (Node Expression)
expressionsInExpression expression =
    let
        expressionsInRecordSetters =
            List.map (\(Node _ ( _, expr )) -> expr)
    in
    case expression of
        LetExpression letBlock ->
            let
                expressionInLetDeclaration (Node _ letDeclaration) =
                    case letDeclaration of
                        LetFunction { declaration } ->
                            declaration |> Node.value |> .expression

                        LetDestructuring _ toDestructure ->
                            toDestructure
            in
            letBlock.declarations
                |> List.map expressionInLetDeclaration
                |> (::) letBlock.expression

        ListExpr expressions ->
            expressions

        TupledExpression expressions ->
            expressions

        RecordExpr setters ->
            setters |> expressionsInRecordSetters

        RecordUpdateExpression record updaters ->
            Node.map val record
                :: (updaters |> expressionsInRecordSetters)

        Application expressions ->
            expressions

        CaseExpression caseBlock ->
            caseBlock.expression
                :: (caseBlock.cases
                        |> List.map (\( _, expr ) -> expr)
                   )

        OperatorApplication _ _ aExpr bExpr ->
            [ aExpr, bExpr ]

        IfBlock boolExpr thenExpr elseExpr ->
            [ boolExpr, thenExpr, elseExpr ]

        LambdaExpression lambda ->
            [ lambda.expression ]

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


{-| There are places where only variable patterns can be, not any pattern:

  - `VarAfterAs`: `(... as here)`

  - `FieldPattern`: `{ here }`

  - `AnnotatedLetVar`:

        let ... : ...
            here = ...
        in

Here, it can be any pattern:

  - `SingleVarPattern`: `( here, _ )` or `Just here` or ...

-}
type VarPatternKind
    = VarAfterAs
    | FieldPattern
    | AnnotatedLetVar
    | SingleVarPattern


{-| Recursively find all variable patterns in a pattern. Also save whether the variable is from after `as`.

    somePattern =
        TuplePattern
            [ UnitPattern |> Node ...
            , VarPattern "iAmHere" |> Node ...
            ]

    pattern
        |> Node ...
        |> collectVarsFromPattern
    --> [ { name = "iAmHere", pattern = pattern } ]

-}
allVarsInPattern :
    Node Pattern
    ->
        List
            { name : Node String
            , kind : VarPatternKind
            }
allVarsInPattern pattern =
    let
        step =
            List.concatMap
                allVarsInPattern

        ofKind kind varPattern =
            { name = varPattern
            , kind = kind
            }
    in
    case Node.value pattern of
        ListPattern elementPatterns ->
            elementPatterns |> step

        TuplePattern subPatterns ->
            subPatterns |> step

        RecordPattern fieldPatterns ->
            fieldPatterns
                |> List.map (ofKind FieldPattern)

        NamedPattern _ subPatterns ->
            subPatterns |> step

        UnConsPattern headPattern tailPattern ->
            [ headPattern, tailPattern ] |> step

        VarPattern name ->
            [ Node (Node.range pattern) name
                |> ofKind SingleVarPattern
            ]

        AsPattern pattern_ afterAs ->
            ([ pattern_ ] |> step)
                |> (::) (afterAs |> ofKind VarAfterAs)

        ParenthesizedPattern innerPattern ->
            [ innerPattern ] |> step

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


{-| Count the uses of a given variable.
-}
usesIn : Expression -> Expression -> number_
usesIn expression toMatch =
    if expression == toMatch then
        1

    else
        expressionsInExpression expression
            |> List.map
                (\(Node _ expr) -> usesIn expr toMatch)
            |> List.sum


{-| `case of` patterns that look like

    case opaque of
        Opaque i ->
            i

can't be used like this in a let destructuring, lambda or argument:

    unpack Opaque i =
        i

so run `parensAroundNamedPattern` to turn possible `NamedPattern`s into `ParenthesizedPattern`.

    unpack (Opaque i) =
        i

-}
parensAroundNamedPattern : Pattern -> Pattern
parensAroundNamedPattern casePattern =
    case casePattern of
        NamedPattern _ _ ->
            parensPattern casePattern

        _ ->
            casePattern
