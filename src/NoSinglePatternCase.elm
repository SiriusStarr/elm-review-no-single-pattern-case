module NoSinglePatternCase exposing (rule)

{-|

@docs rule

-}

import Elm.CodeGen exposing (letDestructuring, letExpr)
import Elm.Pretty exposing (prettyExpression)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Case, Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Pretty exposing (pretty)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)
import SyntaxHelp exposing (collectVarsFromPattern, expressionsInExpression, parensAroundNamedPattern, prettyPrintPattern, variableCountIn)


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
    List
        { name : Node String
        , pattern : Pattern
        , scope : Expression
        }
    -> Node Expression
    -> List (Error {})
checkEpression vars expressionNode =
    case Node.value expressionNode of
        CaseExpression { cases, expression } ->
            case cases of
                [ singleCase ] ->
                    [ singlePatternError
                        { expressionInCaseOf = expression
                        , singleCase = singleCase
                        , patternVars = vars
                        }
                        (Node.range expressionNode)
                    ]

                multipleCases ->
                    multipleCases
                        |> List.concatMap
                            (\( _, expr ) ->
                                expr |> checkEpression vars
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
                                    (arguments
                                        |> List.concatMap collectVarsFromPattern
                                        |> List.map inLetBlockScope
                                        |> (++) vars
                                    )

                        LetDestructuring destructuringPattern toDestructure ->
                            toDestructure
                                |> checkEpression
                                    (destructuringPattern
                                        |> collectVarsFromPattern
                                        |> List.map inLetBlockScope
                                        |> (++) vars
                                    )
            in
            letBlock.expression
                |> checkEpression vars
                |> (++)
                    (letBlock.declarations
                        |> List.concatMap
                            (Node.value
                                >> checkLetDeclaration
                            )
                    )

        otherExpression ->
            expressionsInExpression otherExpression
                |> List.concatMap (checkEpression vars)


singlePatternError :
    { patternVars :
        List
            { name : Node String
            , pattern : Pattern
            , scope : Expression
            }
    , expressionInCaseOf : Node Expression
    , singleCase : Case
    }
    -> Range
    -> Error {}
singlePatternError { patternVars, expressionInCaseOf, singleCase } range =
    let
        ( Node _ casePattern, Node _ caseExpression ) =
            singleCase

        {-
           If

           - the case-block expression is just 1 value
           - the variable was defined in a pattern (e.g. an argument)
           - the variable is only used there
           - the variable isn't zsed after `as`

           then the fix is

           - replace its var pattern with the pattern used in the case

           Everywhere else:

           - replace with let.

        -}
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
                            if variableCountIn scope (Node.value name) == 1 then
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
            , Fix.replaceRangeBy range
                (caseExpression |> prettyExpr range)
            ]

        letFix () =
            [ Fix.replaceRangeBy range
                (let
                    destructuringPattern =
                        case
                            casePattern
                                |> parensAroundNamedPattern
                        of
                            AllPattern ->
                                caseExpression

                            UnitPattern ->
                                caseExpression

                            pattern ->
                                letExpr
                                    [ letDestructuring pattern
                                        (expressionInCaseOf |> Node.value)
                                    ]
                                    caseExpression
                 in
                 destructuringPattern |> prettyExpr range
                )
            ]
    in
    Rule.errorWithFix
        { message = "Single pattern case block."
        , details = [ "Single pattern case blocks are either unnecessary or overly verbose.  There's usually a more concise way to destructure, e.g. in a function argument, so consider refactoring." ]
        }
        range
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
