module NoSinglePatternCase exposing (rule)

{-|

@docs rule

-}

import Elm.CodeGen exposing (letDestructuring, letExpr, val, varPattern)
import Elm.Pretty exposing (prettyExpression, prettyPattern)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Pretty exposing (pretty)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)
import SyntaxHelp exposing (VarPatternKind(..), allVarsInPattern, expressionsInExpression, parensAroundNamedPattern, usesIn)


{-| Reports single-pattern case expressions, which may be written more concisely
or removed entirely.

    config =
        [ NoSinglePatternCase.rule
        ]


## Fail

Single-pattern case expressions for destructuring are not allowed, as:

    type Opaque
        = Opaque Int

    unpack : Opaque -> Int
    unpack o =
        case o of
            Opaque i ->
                i

may instead be written more concisely as:

    unpack : Opaque -> Int
    unpack (Opaque i) =
        i

Similarly, single-pattern case expressions that ase not used for destructuring
are not allowed, as:

    type AOrB
        = A
        | B

    pointless : AOrB -> Bool
    pointless aOrB =
        case aOrB of
            _ ->
                True

may instead be written more concisely as:

    pointless : AOrB -> Bool
    pointless _ =
        True

or

    pointless : AOrB -> Bool
    pointless =
        always True


## Success

Any case expression with more than one pattern match will not be reported.
Consider using [`jfmengels/elm-review-simplify`](https://package.elm-lang.org/packages/jfmengels/elm-review-simplify/latest)
to detect unnecessary multi-pattern cases.


## When (not) to enable this rule

This rule is useful if you prefer destructuring in function/lambda arguments or
`let` bindings, rather than in a single-pattern case.

This rule is not useful if you prefer the more verbose style.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template SiriusStarr/elm-review-no-single-pattern-case/example --rules NoSinglePatternCase
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoSinglePatternCase" ()
        |> Rule.withSimpleDeclarationVisitor
            (Node.value >> checkDeclaration)
        |> Rule.fromModuleRuleSchema


checkDeclaration : Declaration -> List (Error {})
checkDeclaration declaration =
    case declaration of
        FunctionDeclaration fun ->
            let
                { expression, arguments } =
                    fun.declaration |> Node.value
            in
            expression
                |> checkEpression
                    (arguments
                        |> List.concatMap allVarsInPattern
                        |> List.map
                            (inScope (Node.value expression))
                    )

        _ ->
            []


inScope :
    scope
    -> { name : name, nameRange : Range, kind : kind }
    -> { name : name, nameRange : Range, kind : kind, scope : scope }
inScope scope { name, nameRange, kind } =
    { name = name
    , nameRange = nameRange
    , kind = kind
    , scope = scope
    }


checkEpression :
    List
        { name : String
        , nameRange : Range
        , kind : VarPatternKind
        , scope : Expression
        }
    -> Node Expression
    -> List (Error {})
checkEpression vars expressionNode =
    let
        checkExpressionHere extraPatterns =
            checkEpression
                (extraPatterns
                    |> List.map (inScope (Node.value expressionNode))
                    |> (++) vars
                )
    in
    case Node.value expressionNode of
        CaseExpression caseBlock ->
            case caseBlock.cases of
                [ ( singleCasePattern, Node _ singleCaseExpression ) ] ->
                    [ singlePatternCaseError
                        { patternVars = vars
                        , expressionInCaseOf =
                            caseBlock.expression |> Node.value
                        , singleCasePattern = singleCasePattern
                        , singleCaseExpression = singleCaseExpression
                        , caseRange = Node.range expressionNode
                        }
                    ]

                multipleCases ->
                    multipleCases
                        |> List.concatMap
                            (\( _, expr ) ->
                                expr |> checkExpressionHere []
                            )

        LetExpression letBlock ->
            let
                newVarsInLetDeclaration (Node _ letDeclaration) =
                    case letDeclaration of
                        LetDestructuring pattern _ ->
                            pattern |> allVarsInPattern

                        LetFunction fun ->
                            let
                                declaration =
                                    fun.declaration |> Node.value
                            in
                            { name = declaration.name |> Node.value
                            , nameRange = declaration.name |> Node.range
                            , kind =
                                case fun.signature of
                                    Just _ ->
                                        AnnotatedLetVar

                                    Nothing ->
                                        SingleVarPattern
                            }
                                |> List.singleton

                newVarsInLetBlock =
                    letBlock.declarations
                        |> List.concatMap newVarsInLetDeclaration

                checkDeclarations =
                    letBlock.declarations
                        |> List.concatMap checkLetDeclaration

                checkLetDeclaration (Node _ letDeclaration) =
                    case letDeclaration of
                        LetFunction fun ->
                            let
                                declaration =
                                    fun.declaration |> Node.value
                            in
                            declaration.expression
                                |> checkExpressionHere
                                    (newVarsInLetBlock
                                        ++ (declaration.arguments
                                                |> List.concatMap allVarsInPattern
                                           )
                                    )

                        LetDestructuring _ expr ->
                            expr |> checkExpressionHere newVarsInLetBlock
            in
            letBlock.expression
                |> checkExpressionHere []
                |> (++) checkDeclarations

        otherExpression ->
            expressionsInExpression otherExpression
                |> List.concatMap (checkExpressionHere [])


{-| An error for when a case expression only contains one case pattern.


#### fix

If

  - the case-block expression is just 1 value, for example not

        case functionCall o of

  - the variable was defined in a pattern (e.g. an argument), for example not

        import Somewhere exposing (imported)
        case imported of

        topLevel : Opaque
        case topLevel of

        let
            withAnnotation : Opaque
        in
        case withAnnotation of

  - the variable is only used there, for example not

        let
            something =
                alsoUse o
        in
        case o of

  - the variable isn't a record field or after `as`, for example not

        unpack ((Opaque i) as o) =
            case o of

        unpack { o } =
            case o of

then the fix is: Replace its variable pattern with the pattern used in the case.

Everywhere else: Destructure in a let instead.

If there are already let declarations directly before the case

    let
        something =
            alsoUse o
    in
    case o of
        Opaque i ->
            i

the resulting `let`s

    let
        something =
            alsoUse o
    in
    let
        (Opaque i) =
            o
    in
    i

are not joined into one because of situations like

    let
        --        ↓ name clash
        something i =
            alsoUse o

        (Opaque i) =
            o
    in
    i

-}
singlePatternCaseError :
    { patternVars :
        List
            { name : String
            , nameRange : Range
            , kind : VarPatternKind
            , scope : Expression
            }
    , expressionInCaseOf : Expression
    , singleCasePattern : Node Pattern
    , singleCaseExpression : Expression
    , caseRange : Range
    }
    -> Error {}
singlePatternCaseError { patternVars, expressionInCaseOf, singleCaseExpression, singleCasePattern, caseRange } =
    let
        info =
            { message = "Single pattern case block."
            , details = [ "Single pattern case blocks are either unnecessary or overly verbose.  There's usually a more concise way to destructure, e.g. in a function argument, so consider refactoring." ]
            }

        fix =
            case expressionInCaseOf of
                FunctionOrValue [] varName ->
                    case
                        patternVars
                            |> List.filter
                                (.name >> (==) varName)
                    of
                        varPatternInCaseOf :: _ ->
                            case varPatternInCaseOf.kind of
                                SingleVarPattern ->
                                    case
                                        usesIn varPatternInCaseOf.scope
                                            varPatternInCaseOf.name
                                    of
                                        1 ->
                                            if
                                                allVarsInPattern singleCasePattern
                                                    |> List.all
                                                        (.name
                                                            >> usesIn varPatternInCaseOf.scope
                                                            >> (==) 1
                                                        )
                                            then
                                                replaceVarPatternFix varPatternInCaseOf.nameRange

                                            else
                                                fixWithSeperateLet ()

                                        _ ->
                                            fixWithSeperateLet ()

                                VarAfterAs ->
                                    fixWithSeperateLet ()

                                FieldPattern ->
                                    fixWithSeperateLet ()

                                AnnotatedLetVar ->
                                    fixWithSeperateLet ()

                        _ ->
                            fixWithSeperateLet ()

                _ ->
                    fixWithSeperateLet ()

        replaceVarPatternFix varRange =
            [ Fix.replaceRangeBy varRange
                (singleCasePattern
                    |> Node.value
                    |> parensAroundNamedPattern
                    |> prettyPrintPattern
                )
            , Fix.replaceRangeBy caseRange
                (singleCaseExpression |> prettyExpr caseRange)
            ]

        fixWithSeperateLet () =
            let
                fixUseless =
                    [ singleCaseExpression
                        |> prettyExpr caseRange
                        |> Fix.replaceRangeBy caseRange
                    ]
            in
            case singleCasePattern |> Node.value of
                AllPattern ->
                    fixUseless

                UnitPattern ->
                    fixUseless

                pattern ->
                    [ letExpr
                        [ letDestructuring
                            (pattern |> parensAroundNamedPattern)
                            expressionInCaseOf
                        ]
                        singleCaseExpression
                        |> prettyExpr caseRange
                        |> Fix.replaceRangeBy caseRange
                    ]
    in
    Rule.errorWithFix info caseRange fix


prettyExpr : Range -> Expression -> String
prettyExpr replacedRange =
    prettyExpression
        >> pretty 120
        >> adaptIndentation replacedRange


adaptIndentation : Range -> String -> String
adaptIndentation previousRange =
    -- hacky but works
    let
        indentation =
            previousRange.start.column - 1
    in
    String.split "\n"
        >> String.join
            ("\n" ++ String.repeat indentation " ")


{-| If you call `Elm.Pretty.prettyPattern` on `ParenthesizedPattern inner`, `inner` is the result.
This method displays the actual `(inner)`.
-}
prettyPrintPattern : Pattern -> String
prettyPrintPattern pattern =
    case pattern of
        ParenthesizedPattern (Node _ inParens) ->
            "(" ++ (inParens |> prettyPrintPattern) ++ ")"

        _ ->
            pattern |> prettyPattern |> pretty 120
