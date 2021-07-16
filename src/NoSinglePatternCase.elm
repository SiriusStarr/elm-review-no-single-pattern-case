module NoSinglePatternCase exposing (rule)

{-|

@docs rule

-}

import Elm.CodeGen exposing (letDestructuring, letExpr)
import Elm.Pretty exposing (prettyExpression, prettyPattern)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Case, Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Pretty exposing (pretty)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)
import SyntaxHelp exposing (collectVarsFromPattern, expressionsInExpression, parensAroundNamedPattern, variableUsageCountIn)


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
                (Node _ { expression, arguments }) =
                    fun.declaration
            in
            expression
                |> checkEpression
                    { letDirectlyBefore = Nothing }
                    (arguments
                        |> List.concatMap collectVarsFromPattern
                        |> List.map
                            (inScope (Node.value expression))
                    )

        _ ->
            []


inScope :
    scope
    -> { record | name : name, pattern : pattern }
    -> { name : name, pattern : pattern, scope : scope }
inScope scope { name, pattern } =
    { name = name
    , pattern = pattern
    , scope = scope
    }


checkEpression :
    { letDirectlyBefore :
        Maybe { declarations : List LetDeclaration, range : Range }
    }
    ->
        List
            { name : Node String
            , pattern : Pattern
            , scope : Expression
            }
    -> Node Expression
    -> List (Error {})
checkEpression { letDirectlyBefore } vars expressionNode =
    case Node.value expressionNode of
        CaseExpression { cases, expression } ->
            case cases of
                [ singleCase ] ->
                    [ singlePatternError
                        { patternVars = vars
                        , expressionInCaseOf = expression
                        , singleCase = singleCase
                        , letDirectlyBefore = letDirectlyBefore
                        , caseRange = expressionNode |> Node.range
                        }
                    ]

                multipleCases ->
                    multipleCases
                        |> List.concatMap
                            (\( _, expr ) ->
                                expr
                                    |> checkEpression
                                        { letDirectlyBefore = Nothing }
                                        vars
                            )

        LetExpression letBlock ->
            let
                checkLetDeclaration letDeclaration =
                    let
                        inLetBlockScope =
                            inScope (Node.value letBlock.expression)
                    in
                    case letDeclaration of
                        LetFunction { declaration } ->
                            let
                                { expression, arguments } =
                                    Node.value declaration
                            in
                            expression
                                |> checkEpression
                                    { letDirectlyBefore = Nothing }
                                    (arguments
                                        |> List.concatMap collectVarsFromPattern
                                        |> List.map inLetBlockScope
                                        |> (++) vars
                                    )

                        LetDestructuring destructuringPattern toDestructure ->
                            toDestructure
                                |> checkEpression
                                    { letDirectlyBefore = Nothing }
                                    (destructuringPattern
                                        |> collectVarsFromPattern
                                        |> List.map inLetBlockScope
                                        |> (++) vars
                                    )
            in
            letBlock.expression
                |> checkEpression
                    { letDirectlyBefore =
                        { declarations =
                            letBlock.declarations
                                |> List.map Node.value
                        , range = Node.range expressionNode
                        }
                            |> Just
                    }
                    vars
                |> (++)
                    (letBlock.declarations
                        |> List.concatMap
                            (Node.value
                                >> checkLetDeclaration
                            )
                    )

        otherExpression ->
            expressionsInExpression otherExpression
                |> List.concatMap
                    (checkEpression
                        { letDirectlyBefore = Nothing }
                        vars
                    )


{-|


#### fixes

If

  - the case-block expression is just 1 value, for example not

        case functionCall o of

  - the variable was defined in a pattern (e.g. an argument), for example not

        import Somewhere exposing (o)

        case o of

  - the variable is only used there, for example not

        let
            something =
                alsoUse o
        in
        case o of

  - the variable isn't after `as`, for example not

        unpack ((Opaque i) as o) =
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

are joined into one.

    let
        something =
            alsoUse o

        (Opaque i) =
            o
    in
    i

-}
singlePatternError :
    { patternVars :
        List
            { name : Node String
            , pattern : Pattern
            , scope : Expression
            }
    , expressionInCaseOf : Node Expression
    , singleCase : Case
    , letDirectlyBefore :
        Maybe { declarations : List LetDeclaration, range : Range }
    , caseRange : Range
    }
    -> Error {}
singlePatternError { patternVars, expressionInCaseOf, singleCase, letDirectlyBefore, caseRange } =
    let
        ( Node _ casePattern, Node _ caseExpression ) =
            singleCase

        fix =
            let
                patternVarsWithoutAfterAs =
                    patternVars
                        |> List.filter
                            (\{ pattern } ->
                                case pattern of
                                    AsPattern _ _ ->
                                        False

                                    _ ->
                                        True
                            )
            in
            case Node.value expressionInCaseOf of
                FunctionOrValue [] varName ->
                    case
                        patternVarsWithoutAfterAs
                            |> List.filter
                                (.name >> Node.value >> (==) varName)
                    of
                        { name, scope } :: _ ->
                            if variableUsageCountIn scope (Node.value name) == 1 then
                                replaceVarPatternFix name

                            else
                                letFix ()

                        _ ->
                            letFix ()

                _ ->
                    letFix ()

        replaceVarPatternFix varPattern =
            [ Fix.replaceRangeBy (Node.range varPattern)
                (casePattern
                    |> parensAroundNamedPattern
                    |> prettyPrintPattern
                )
            , Fix.replaceRangeBy caseRange
                (caseExpression |> prettyExpr caseRange)
            ]

        letFix () =
            [ let
                fixUseless =
                    caseExpression
                        |> prettyExpr caseRange
                        |> Fix.replaceRangeBy caseRange
              in
              case casePattern of
                AllPattern ->
                    fixUseless

                UnitPattern ->
                    fixUseless

                pattern ->
                    let
                        replaceWithLetBlock rangeToReplace { extraDeclarationsBefore } =
                            letExpr
                                (extraDeclarationsBefore
                                    ++ [ letDestructuring
                                            (pattern |> parensAroundNamedPattern)
                                            (expressionInCaseOf |> Node.value)
                                       ]
                                )
                                caseExpression
                                |> prettyExpr rangeToReplace
                                |> Fix.replaceRangeBy rangeToReplace
                    in
                    case letDirectlyBefore of
                        Just let_ ->
                            replaceWithLetBlock let_.range
                                { extraDeclarationsBefore = let_.declarations }

                        Nothing ->
                            replaceWithLetBlock caseRange
                                { extraDeclarationsBefore = [] }
            ]
    in
    Rule.errorWithFix
        { message = "Single pattern case block."
        , details = [ "Single pattern case blocks are either unnecessary or overly verbose.  There's usually a more concise way to destructure, e.g. in a function argument, so consider refactoring." ]
        }
        caseRange
        fix


prettyExpr : Range -> Expression -> String
prettyExpr replacedRange =
    prettyExpression
        >> pretty 120
        >> adaptIndentation replacedRange


adaptIndentation : Range -> String -> String
adaptIndentation previousRange =
    let
        -- hacky but works
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
        ParenthesizedPattern inParens ->
            "(" ++ prettyPrintPattern (Node.value inParens) ++ ")"

        _ ->
            pattern |> prettyPattern |> pretty 120
