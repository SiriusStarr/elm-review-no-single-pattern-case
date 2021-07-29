module SyntaxHelp exposing (VarPatternKind(..), allVarsInPattern, expressionsInExpression, parensAroundNamedPattern, prettyPrintPattern, updateExpressionsInExpression, usesIn)

import Elm.CodeGen exposing (parensPattern, val)
import Elm.Pretty exposing (prettyPattern)
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Pretty exposing (pretty)


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


updateExpressionsInExpression : (Node Expression -> Node Expression) -> Expression -> Expression
updateExpressionsInExpression update expression =
    let
        updateExpressionsInRecordSetters =
            List.map
                (Node.map
                    (\( field, expr ) ->
                        ( field, expr |> update )
                    )
                )
    in
    case expression of
        LetExpression letBlock ->
            let
                updateExpressionInLetDeclaration letDeclaration =
                    case letDeclaration of
                        LetFunction fun ->
                            LetFunction
                                { fun
                                    | declaration =
                                        fun.declaration
                                            |> Node.map
                                                (\f ->
                                                    { f
                                                        | expression =
                                                            f.expression
                                                                |> update
                                                    }
                                                )
                                }

                        LetDestructuring pattern toDestructure ->
                            LetDestructuring pattern (toDestructure |> update)
            in
            LetExpression
                { declarations =
                    letBlock.declarations
                        |> List.map
                            (Node.map updateExpressionInLetDeclaration)
                , expression =
                    letBlock.expression |> update
                }

        ListExpr expressions ->
            ListExpr
                (expressions |> List.map update)

        TupledExpression expressions ->
            TupledExpression
                (expressions |> List.map update)

        RecordExpr setters ->
            RecordExpr
                (setters |> updateExpressionsInRecordSetters)

        RecordUpdateExpression record updaters ->
            RecordUpdateExpression record
                (updaters |> updateExpressionsInRecordSetters)

        Application expressions ->
            Application (expressions |> List.map update)

        CaseExpression caseBlock ->
            CaseExpression
                { expression = caseBlock.expression |> update
                , cases =
                    caseBlock.cases
                        |> List.map
                            (\( pattern, expr ) ->
                                ( pattern, expr |> update )
                            )
                }

        OperatorApplication name dir aExpr bExpr ->
            OperatorApplication name
                dir
                (aExpr |> update)
                (bExpr |> update)

        IfBlock boolExpr thenExpr elseExpr ->
            IfBlock (boolExpr |> update)
                (thenExpr |> update)
                (elseExpr |> update)

        LambdaExpression lambda ->
            LambdaExpression
                { lambda
                    | expression =
                        lambda.expression |> update
                }

        RecordAccess record fieldName ->
            RecordAccess (record |> update) fieldName

        ParenthesizedExpression expr ->
            ParenthesizedExpression (expr |> update)

        Negation expr ->
            Negation (expr |> update)

        UnitExpr ->
            expression

        Integer _ ->
            expression

        Hex _ ->
            expression

        Floatable _ ->
            expression

        Literal _ ->
            expression

        CharLiteral _ ->
            expression

        GLSLExpression _ ->
            expression

        RecordAccessFunction _ ->
            expression

        FunctionOrValue _ _ ->
            expression

        Operator _ ->
            expression

        PrefixOperator _ ->
            expression


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
            { name : String
            , nameRange : Range
            , kind : VarPatternKind
            }
allVarsInPattern pattern =
    let
        step =
            List.concatMap
                allVarsInPattern

        ofKind kind varPattern =
            { name = Node.value varPattern
            , nameRange = Node.range varPattern
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
            [ { name = name
              , nameRange = Node.range pattern
              , kind = SingleVarPattern
              }
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


{-| Count the uses of a given name (defined variables with this name) in the scope of the expression.
-}
usesIn : Expression -> String -> number_
usesIn expression matchName =
    case expression of
        FunctionOrValue _ name ->
            if name == matchName then
                1

            else
                0

        _ ->
            expressionsInExpression expression
                |> List.map
                    (\(Node _ expr) ->
                        usesIn expr matchName
                    )
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


{-| If you call `Elm.Pretty.prettyPattern` on `ParenthesizedPattern inner`, `inner` is the result.
This method displays the actual `(inner)`. It's the same with `AsPattern`.
-}
prettyPrintPattern : Int -> Pattern -> String
prettyPrintPattern width pattern =
    let
        printedPattern =
            pattern |> prettyPattern |> pretty width

        inParens =
            "(" ++ printedPattern ++ ")"
    in
    case pattern of
        ParenthesizedPattern _ ->
            inParens

        AsPattern _ _ ->
            inParens

        _ ->
            printedPattern
