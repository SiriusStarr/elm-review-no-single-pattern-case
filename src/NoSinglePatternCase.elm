module NoSinglePatternCase exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Error, Rule)


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

This rule is useful if you prefer destructuring in e.g. function arguments or
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
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Node.value node of
        Expression.CaseExpression { cases } ->
            case cases of
                [ _ ] ->
                    [ singlePatternError <| Node.range node ]

                _ ->
                    []

        _ ->
            []


singlePatternError : Range -> Error {}
singlePatternError =
    Rule.error
        { message = "Single pattern case block."
        , details = [ "Single pattern case blocks are either unnecessary or overly verbose.  There's usually a more concise way to destructure, e.g. in a function argument, so consider refactoring." ]
        }
