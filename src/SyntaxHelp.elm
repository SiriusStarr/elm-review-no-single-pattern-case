module SyntaxHelp exposing (collectVarsFromPattern, expressionsInExpression, letDeclarationArguments, parensAroundNamedPattern, prettyPrintPattern, variableCountIn)

import Elm.CodeGen exposing (parensPattern)
import Elm.Pretty exposing (prettyPattern)
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Pretty exposing (pretty)


letDeclarationArguments : LetDeclaration -> List (Node Pattern)
letDeclarationArguments letDeclaration =
    case letDeclaration of
        LetFunction { declaration } ->
            declaration |> Node.value |> .arguments

        LetDestructuring pattern _ ->
            [ pattern ]


{-| not recursive. Just the direct child expressions.
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
                :: (letBlock.declarations
                        |> List.map
                            (\letDeclaration ->
                                case Node.value letDeclaration of
                                    LetFunction { declaration } ->
                                        declaration |> Node.value |> .expression

                                    LetDestructuring _ toDestructure ->
                                        toDestructure
                            )
                   )

        ListExpr expressions ->
            expressions

        TupledExpression expressions ->
            expressions

        RecordExpr setters ->
            setters |> collectRecordSetterExpressions

        RecordUpdateExpression _ updaters ->
            updaters |> collectRecordSetterExpressions

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
            [ name |> Node (Node.range pattern) |> withPattern ]

        AsPattern pattern_ what ->
            (what |> withPattern)
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


variableCountIn : Expression -> String -> number_
variableCountIn expression variable =
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
                        variableCountIn
                            (Node.value expr)
                            variable
                    )
                |> List.sum


parensAroundNamedPattern : Pattern -> Pattern
parensAroundNamedPattern casePattern =
    case casePattern of
        NamedPattern name patterns ->
            parensPattern (NamedPattern name patterns)

        _ ->
            casePattern


{-| If you call `Elm.Pretty.prettyPattern` on `ParenthesizedPattern inner`, `inner` is the result.
This method displays the actual `(inner)`.
-}
prettyPrintPattern : Pattern -> String
prettyPrintPattern pattern =
    case pattern of
        ParenthesizedPattern inParens ->
            "(" ++ prettyPrintPattern (Node.value inParens) ++ ")"

        _ ->
            pattern |> prettyPattern |> pretty 120
