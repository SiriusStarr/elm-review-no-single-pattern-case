module SyntaxHelp exposing (collectVarsFromPattern, expressionsInExpression, parensAroundNamedPattern, variableUsageIn)

import Elm.CodeGen exposing (parensPattern, val)
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))


{-| The direct child expressions.
-}
expressionsInExpression : Expression -> List (Node Expression)
expressionsInExpression expression =
    let
        collectRecordSetterExpressions =
            List.map Node.value
                >> List.map (\( _, expr ) -> expr)
    in
    case expression of
        LetExpression letBlock ->
            letBlock.expression
                :: (let
                        expressionInLetDeclaration (Node _ letDeclaration) =
                            case letDeclaration of
                                LetFunction { declaration } ->
                                    declaration |> Node.value |> .expression

                                LetDestructuring _ toDestructure ->
                                    toDestructure
                    in
                    letBlock.declarations
                        |> List.map expressionInLetDeclaration
                   )

        ListExpr expressions ->
            expressions

        TupledExpression expressions ->
            expressions

        RecordExpr setters ->
            setters |> collectRecordSetterExpressions

        RecordUpdateExpression record updaters ->
            Node.map val record
                :: (updaters |> collectRecordSetterExpressions)

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


collectVarsFromPattern :
    Node Pattern
    ->
        List
            { name : Node String
            , pattern : Pattern
            }
collectVarsFromPattern pattern =
    let
        step =
            List.concatMap
                collectVarsFromPattern

        withPattern varPattern =
            { name = varPattern
            , pattern = Node.value pattern
            }
    in
    case Node.value pattern of
        ListPattern elementPatterns ->
            elementPatterns |> step

        TuplePattern subPatterns ->
            subPatterns |> step

        RecordPattern fieldPatterns ->
            fieldPatterns |> List.map withPattern

        NamedPattern _ subPatterns ->
            subPatterns |> step

        UnConsPattern headPattern tailPattern ->
            [ headPattern, tailPattern ] |> step

        VarPattern name ->
            [ Node (Node.range pattern) name
                |> withPattern
            ]

        AsPattern pattern_ afterAs ->
            (afterAs |> withPattern)
                :: ([ pattern_ ] |> step)

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
variableUsageIn : Expression -> String -> number_
variableUsageIn expression variable =
    case expression of
        FunctionOrValue [] var ->
            if var == variable then
                1

            else
                0

        _ ->
            expressionsInExpression expression
                |> List.map
                    (\expr ->
                        variableUsageIn
                            (Node.value expr)
                            variable
                    )
                |> List.sum


{-| `case of` patterns that look like

    case opaque of
        Opaque i ->
            i

can't be used like this in a let destructuring, lambda or argument:

    function Opaque i =
        i

so run `parensAroundNamedPattern` to turn possible `NamedPattern`s into `ParenthesizedPattern`.

    function (Opaque i) =
        i

-}
parensAroundNamedPattern : Pattern -> Pattern
parensAroundNamedPattern casePattern =
    case casePattern of
        NamedPattern _ _ ->
            parensPattern casePattern

        _ ->
            casePattern
