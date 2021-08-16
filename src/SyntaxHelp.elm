module SyntaxHelp exposing
    ( Binding
    , addParensToNamedPattern
    , allBindingsInPattern
    , countUsesIn
    , mapSubexpressions
    , prettyExpressionReplacing
    , prettyPrintPattern
    , subexpressions
    )

import Elm.CodeGen exposing (parensPattern, val)
import Elm.Pretty exposing (prettyExpression, prettyPattern)
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Pretty exposing (pretty)


{-| Get all immediate child expressions of an expression.
-}
subexpressions : Expression -> List (Node Expression)
subexpressions e =
    case e of
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
            Node.map val record
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


{-| Map all immediate child expressions of an expression.
-}
mapSubexpressions : (Node Expression -> Node Expression) -> Expression -> Expression
mapSubexpressions f e =
    case e of
        LetExpression { declarations, expression } ->
            let
                mapSubexprs : LetDeclaration -> LetDeclaration
                mapSubexprs d =
                    case d of
                        LetFunction fun ->
                            LetFunction
                                { fun
                                    | declaration =
                                        Node.map
                                            (\impl ->
                                                { impl
                                                    | expression =
                                                        f impl.expression
                                                }
                                            )
                                            fun.declaration
                                }

                        LetDestructuring pattern expr ->
                            LetDestructuring pattern (f expr)
            in
            LetExpression
                { declarations =
                    List.map
                        (Node.map mapSubexprs)
                        declarations
                , expression =
                    f expression
                }

        ListExpr exprs ->
            ListExpr (List.map f exprs)

        TupledExpression exprs ->
            TupledExpression (List.map f exprs)

        RecordExpr setters ->
            RecordExpr (List.map (Node.map (Tuple.mapSecond f)) setters)

        RecordUpdateExpression record updaters ->
            List.map (Node.map (Tuple.mapSecond f)) updaters
                |> RecordUpdateExpression record

        Application exprs ->
            Application (List.map f exprs)

        CaseExpression { expression, cases } ->
            CaseExpression
                { expression = f expression
                , cases = List.map (Tuple.mapSecond f) cases
                }

        OperatorApplication name dir e1 e2 ->
            OperatorApplication name
                dir
                (f e1)
                (f e2)

        IfBlock predExpr thenExpr elseExpr ->
            IfBlock (f predExpr)
                (f thenExpr)
                (f elseExpr)

        LambdaExpression lambda ->
            LambdaExpression { lambda | expression = f lambda.expression }

        RecordAccess record fieldName ->
            RecordAccess (f record) fieldName

        ParenthesizedExpression expr ->
            ParenthesizedExpression (f expr)

        Negation expr ->
            Negation (f expr)

        UnitExpr ->
            e

        Integer _ ->
            e

        Hex _ ->
            e

        Floatable _ ->
            e

        Literal _ ->
            e

        CharLiteral _ ->
            e

        GLSLExpression _ ->
            e

        RecordAccessFunction _ ->
            e

        FunctionOrValue _ _ ->
            e

        Operator _ ->
            e

        PrefixOperator _ ->
            e


{-| Recursively find all bindings in a pattern and save whether or not
destructuring could occur at the pattern.
-}
allBindingsInPattern : Expression -> Node Pattern -> List ( String, Binding )
allBindingsInPattern scope pattern =
    let
        go : List (Node Pattern) -> List ( String, Binding )
        go =
            List.concatMap (allBindingsInPattern scope)

        makeBinding : Bool -> Node String -> ( String, Binding )
        makeBinding canDestructureAt name =
            ( Node.value name
            , { patternNodeRange = Node.range name
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
              , { patternNodeRange = Node.range pattern
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


{-| Count the uses of a given name in the scope of an expression.
-}
countUsesIn : Expression -> String -> number_
countUsesIn expr name =
    case expr of
        -- If the name is qualified, it isn't a variable
        FunctionOrValue [] n ->
            if n == name then
                1

            else
                0

        _ ->
            subexpressions expr
                -- Count and sum in one pass
                |> List.foldl (\e -> (+) (countUsesIn (Node.value e) name)) 0


{-| A binding with some scope.

  - `patternNodeRange` -- The `Range` for the binding location.
  - `canDestructureAt` -- Whether or not the binding can be destructured at.
  - `scope` -- The scope the binding is bound within.

-}
type alias Binding =
    { patternNodeRange : Range
    , canDestructureAt : Bool
    , scope : Expression
    }


{-| Named patterns that occur in e.g. function arguments must be surrounded by
parentheses, unlike in `case` patterns. This function wraps them accordingly,
e.g. for:

    f o =
        case o of
            Opaque i ->
                i

vs

    f (Opaque i) =
        i

-}
addParensToNamedPattern : Pattern -> Pattern
addParensToNamedPattern p =
    case p of
        NamedPattern _ _ ->
            parensPattern p

        _ ->
            p


{-| If you call `Elm.Pretty.prettyPattern` on `ParenthesizedPattern inner`,
`inner` is the result. This method displays the actual `(inner)` instead, as
well as putting parentheses around `as` patterns.
-}
prettyPrintPattern : Int -> Pattern -> String
prettyPrintPattern width pattern =
    let
        printedPattern : String
        printedPattern =
            pattern |> prettyPattern |> pretty width

        inParens : String
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


{-| Replace a range with a provided expression, pretty-printing and indenting
the result.
-}
prettyExpressionReplacing : Range -> Expression -> String
prettyExpressionReplacing replacedRange =
    prettyExpression
        >> pretty 120
        >> reindent replacedRange.start.column


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
