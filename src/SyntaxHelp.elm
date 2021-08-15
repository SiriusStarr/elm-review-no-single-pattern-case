module SyntaxHelp exposing
    ( VarPatternKind(..)
    , allVarsInPattern
    , mapSubexpressions
    , parensAroundNamedPattern
    , prettyPrintPattern
    , subexpressions
    , usesIn
    )

import Elm.CodeGen exposing (parensPattern, val)
import Elm.Pretty exposing (prettyPattern)
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
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
            letBlock.expression :: List.map subExprs letBlock.declarations

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


{-| There are places where only variable patterns can be, not any pattern:

  - `VarAfterAs`: `(... as here)`

  - `FieldPattern`: `{ here }`

  - `AnnotatedLetVar`:

        let
            here : ...
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
        go =
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
            go elementPatterns

        TuplePattern subPatterns ->
            go subPatterns

        RecordPattern fieldPatterns ->
            List.map (ofKind FieldPattern) fieldPatterns

        NamedPattern _ subPatterns ->
            go subPatterns

        UnConsPattern headPattern tailPattern ->
            go [ headPattern, tailPattern ]

        VarPattern name ->
            [ { name = name
              , nameRange = Node.range pattern
              , kind = SingleVarPattern
              }
            ]

        AsPattern pattern_ afterAs ->
            ofKind VarAfterAs afterAs :: go [ pattern_ ]

        ParenthesizedPattern innerPattern ->
            go [ innerPattern ]

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
        -- If the name is qualified, it isn't a variable
        FunctionOrValue [] name ->
            if name == matchName then
                1

            else
                0

        _ ->
            subexpressions expression
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
