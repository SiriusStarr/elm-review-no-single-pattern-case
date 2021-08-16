module NoSinglePatternCase exposing
    ( rule
    , Config
    , CreateSeparateLet, DontCreateSeparateLet
    , alwaysFixInArgument, alwaysFixInLet
    , fixByDestructuringInExistingLets, fixByDestructuringTheArgument
    , destructureInExistingLets, destructureAs, destructureTheArgument, destructureAsInstead, destructureInExistingLetsInstead
    , noFix, createSeparateLet
    , createSeparateLetOnNameClash, noFixOnNameClash
    )

{-|

@docs rule


# configure

@docs Config
@docs CreateSeparateLet, DontCreateSeparateLet


## default

@docs alwaysFixInArgument, alwaysFixInLet


## detailed

@docs fixByDestructuringInExistingLets, fixByDestructuringTheArgument
@docs destructureInExistingLets, destructureAs, destructureTheArgument, destructureAsInstead, destructureInExistingLetsInstead


### separate let is only option left

@docs noFix, createSeparateLet


### on name clash

@docs createSeparateLetOnNameClash, noFixOnNameClash

-}

import Dict exposing (Dict)
import Elm.CodeGen exposing (asPattern, letDestructuring, letExpr)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range, emptyRange)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)
import SyntaxHelp
    exposing
        ( Binding
        , allBindingsInPattern
        , mapSubexpressions
        , parensAroundNamedPattern
        , prettyExpressionReplacing
        , prettyPrintPattern
        , subexpressions
        , usesIn
        )


{-| Reports single-pattern case expressions, which may be written more concisely
or removed entirely.

    config =
        [ NoSinglePatternCase.rule
            NoSinglePatternCase.alwaysFixInArgument
        ]

See [`Config`](NoSinglePatternCase#Config).


## Fail

Single-pattern case expressions for destructuring are not allowed, as:

    type Opaque
        = Opaque Int

    unpack : Opaque -> Int
    unpack o =
        case o of
            Opaque i ->
                i

may instead be written more concisely, for example as

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

may instead be written more concisely, for example as

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
rule : Config separateLetUsed -> Rule
rule config =
    Rule.newModuleRuleSchema "NoSinglePatternCase" ()
        |> Rule.withSimpleDeclarationVisitor
            (Node.value >> checkDeclaration config)
        |> Rule.fromModuleRuleSchema


{-| Configure the rule, determining how automatic fixes are generated.

The default `Config`s
[`alwaysFixInArgument`](NoSingleCasePattern#alwaysFixInArgument) or
[`alwaysFixInLet`](NoSingleCasePattern#alwaysFixInLet) should cover what you would most likely want.

You can also create a more detailed `Config`. One example:

    fixOnlyInExistingLet =
        fixByDestructuringInExistingLets
            { noExistingLets = noFix }
            |> noFixOnNameClash

options:

  - [`fixByDestructuringInExistingLets`](NoSinglePatternCase#fixByDestructuringInExistingLets)
      - | `noExistingLets`
          - [`destructureTheArgument`](NoSinglePatternCase#destructureTheArgument)
              - | If the argument pattern is a record field pattern or `as` destructured, use [_"only separate let fix left"_](#only-separate-let-fix-left)
              - | If in `case` and `of` is not just an argument, use [_"only separate let fix left"_](#only-separate-let-fix-left)
              - | `argumentAlsoUsedElsewhere`
                  - [`destructureAs`](NoSinglePatternCase#destructureAs)
                  - [_"only separate let fix left"_](#only-separate-let-fix-left)
          - [_"only separate let fix left"_](#only-separate-let-fix-left)
  - [`fixByDestructuringTheArgument`](NoSinglePatternCase#fixByDestructuringTheArgument)
      - | `argumentAlsoUsedElsewhere`
          - [`destructureAsInstead`](NoSinglePatternCase#destructureAsInstead)
          - [`destructureInExistingLetsInstead`](NoSinglePatternCase#destructureInExistingLetsInstead)
              - | `noExistingLets`
                  - [`destructureAs`](NoSinglePatternCase#destructureAs)
                  - [_"only separate let fix left"_](#only-separate-let-fix-left)
      - | `notDestructurable` -- If in `case` and `of` is not just an argument or if the argument pattern is a record field pattern or `as` destructured
          - [`destructureInExistingLets`](NoSinglePatternCase#destructureInExistingLets)
              - | If no lets exist, use [_"only separate let fix left"_](#only-separate-let-fix-left)
          - [_"only separate let fix left"_](#only-separate-let-fix-left)

| If names in the destructured pattern clash with existing names:
[`createSeparateLetOnNameClash`](NoSinglePatternCase#createSeparateLetOnNameClash) / [`noFixOnNameClash`](NoSinglePatternCase#noFixOnNameClash)
matching with _"only separate let fix left"_


### Only separate let fix left

The only possible automatic fix at this point is creating a separate let.
All _"only separate let fix left"_ can only be either

  - Create separate let [`createSeparateLet`](NoSinglePatternCase#createSeparateLet) or
  - Don't provide a fix [`noFix`](NoSinglePatternCase#noFix)

-}
type Config separateLetUsed
    = Config
        (Fix separateLetUsed)
        (OnlyFixInSeparateLetLeft
            separateLetUsed
        )


{-| Resolve the cases

  - name clash
  - `OnlyFixInSeparateLetLeftFix`

-}
type OnlyFixInSeparateLetLeft separateLetUsed
    = CreateSeparateLet
    | DontCreateSeparateLet


{-| Phantom type only, saying:
Don't provide a fix if the only option left is creating a separate let.

    noFixOnNameClash :
        ... CreateSeparateLet
        -> Config CreateSeparateLet

    fixByDestructuringInExistingLets
        { noExistingLets = createSeparateLet }
        |> noFixOnNameClash
    --> compile time error

-}
type DontCreateSeparateLet
    = NoFixIfOnlyCreatingSeparateLetLeft Never


{-| Phantom type only, saying:
If the only option left is creating a separate let, do that.

    createSeparateLetOnNameClash :
        ... CreateSeparateLet
        -> Config CreateSeparateLet

    fixByDestructuringInExistingLets
        { noExistingLets = noFix }
        |> createSeparateLetOnNameClash
    --> compile time error

-}
type CreateSeparateLet
    = CreateSeparateLetIfOnlyOptionLeft Never


type OnlyFixInSeparateLetLeftFixOr option separateLetUsed
    = Fix option
    | OnlyFixInSeparateLetLeftFix


{-| Specify how to automatically fix single-pattern cases in specific conditions.
-}
type Fix separateLetUsed
    = FixByDestructuringInExistingLets
        { noExistingLets :
            OnlyFixInSeparateLetLeftFixOr
                (DestructureTheArgument separateLetUsed)
                separateLetUsed
        }
    | FixByDestructuringTheArgument
        { argumentAlsoUsedElsewhere :
            ArgumentAlsoUsedElsewhere separateLetUsed
        , notDestructurable :
            --| If in `case` and `of` isn't just an argument or
            --| record field pattern or `as` destructured
            OnlyFixInSeparateLetLeftFixOr
                -- If no lets exist, use _separateLetUsed_
                DestructureInExistingLets
                separateLetUsed
        }


type DestructureTheArgument separateLetUsed
    = DestructureTheArgument
        { argumentAlsoUsedElsewhere :
            OnlyFixInSeparateLetLeftFixOr
                DestructureUsingAs
                separateLetUsed

        -- If in `case` and `of` is not just an argument, use _separateLetUsed_
        -- if the argument is a record field pattern or `as` destructured, use _separateLetUsed_
        }


type DestructureUsingAs
    = DestructureUsingAs


type DestructureInExistingLets
    = DestructureInExistingLets


type ArgumentAlsoUsedElsewhere separateLetUsed
    = UseAsInstead
    | DestructureInExistingLetsInstead
        { noExistingLets :
            OnlyFixInSeparateLetLeftFixOr
                DestructureUsingAs
                separateLetUsed
        }



--


{-| Fixing a single-pattern case expression by destructuring in either the
function argument or an existing `let` binding can expand the scope of the variable. If
there is already a variable in scope with the same name, this would lead to a
name clash.

    before : Opaque -> Int
    before o =
        let
            clash i =
                i
        in
        case o of
            Opaque i ->
                i

The destructured argument in a new `let` binding has the _same scope as the original `case...of`_.

    after : Opaque -> Int
    after o =
        let
            clash i =
                i
        in
        let
            (Opaque i) =
                o
        in
        i

-}
createSeparateLetOnNameClash :
    Fix CreateSeparateLet
    -> Config CreateSeparateLet
createSeparateLetOnNameClash fix =
    Config fix CreateSeparateLet


{-| Fixing a single-pattern case expression by destructuring in either the
function argument or an existing `let` binding can expand the scope of the variable. If
there is already a variable in scope with the same name, this would lead to a
name clash.

    f : Opaque -> Int
    f o =
        let
            clash i =
                i
        in
        case o of
            Opaque i ->
                i

Using `noFixOnNameClash`, no automatic fix is provided, allowing you to resolve the name clash manually.

-}
noFixOnNameClash :
    Fix DontCreateSeparateLet
    -> Config DontCreateSeparateLet
noFixOnNameClash fix =
    Config fix DontCreateSeparateLet


{-| Destructure the argument after existing let declarations.

    before o =
        let
            thing =
                ...
        in
        case o of
            Opaque i ->
                i

    after o =
        let
            thing =
                ...

            (Opaque i) =
                o
        in
        i

options:

  - | `noExistingLets`
      - [`destructureTheArgument`](NoSinglePatternCase#destructureTheArgument)
          - | If the var pattern is a record field pattern or `as` destructured, use your fix for the situation "only separate let fix left"
          - | If in `case` and `of` is not just an argument, use your fix for the situation "only separate let fix left"
          - | `argumentAlsoUsedElsewhere`
              - [`destructureAs`](NoSinglePatternCase#destructureAs)
              - your fix for the situation "only separate let fix left"
      - your fix for the situation "only separate let fix left"

examples

    fixByDestructuringInExistingLets
        { noExistingLets =
            destructureTheArgument
                { argumentAlsoUsedElsewhere =
                    destructureAs
                }
        }

    onlyFixInExistingLets =
        fixByDestructuringInExistingLets
            { noExistingLets = noFix }
            |> noFixOnNameClash

-}
fixByDestructuringInExistingLets :
    { noExistingLets :
        OnlyFixInSeparateLetLeftFixOr
            (DestructureTheArgument separateLetUsed)
            separateLetUsed
    }
    -> Fix separateLetUsed
fixByDestructuringInExistingLets noExistingLets =
    FixByDestructuringInExistingLets noExistingLets


{-| Destructure the argument.

    before o =
        case o of
            Opaque i ->
                i

    after (Opaque i) =
        i

options:

  - | `argumentAlsoUsedElsewhere`
      - [`destructureAsInstead`](NoSinglePatternCase#destructureAsInstead)
      - [`destructureInExistingLetsInstead`](NoSinglePatternCase#destructureInExistingLetsInstead)
          - | `noExistingLets`
              - [`destructureAs`](NoSinglePatternCase#destructureAs)
              - your fix for the situation "only separate let fix left"
  - | `notDestructurable` -- If the expression in `case` and `of` is not an argument or the argument pattern is a record field pattern or `as` destructured
      - [`destructureInExistingLets`](NoSinglePatternCase#destructureInExistingLets)
          - | If no lets exist, use your fix for the situation "only separate let fix left"
      - your fix for the situation "only separate let fix left"

For example

    destructureInArgumentElseInLet =
        fixByDestructuringTheArgument
            { argumentAlsoUsedElsewhere =
                destructureInExistingLetsInstead
                    { noExistingLets = createSeparateLet }
            , notDestructurable =
                destructureInExistingLets
            }
            |> createSeparateLetOnNameClash

-}
fixByDestructuringTheArgument :
    { argumentAlsoUsedElsewhere :
        ArgumentAlsoUsedElsewhere separateLetUsed
    , notDestructurable :
        OnlyFixInSeparateLetLeftFixOr
            DestructureInExistingLets
            separateLetUsed
    }
    -> Fix separateLetUsed
fixByDestructuringTheArgument exceptions =
    FixByDestructuringTheArgument exceptions


{-| Destructure after existing let declarations.

    before o =
        let
            foo =
                ...
        in
        case o of
            Opaque i ->
                f i foo

    after o =
        let
            foo =
                ...

            (Opaque i) =
                o
        in
        f i foo

  - | If no lets exist, uses your fix for the situation "only separate let fix left".

For example

    fixByDestructuringTheArgument
        { argumentAlsoUsedElsewhere = destructureAsInstead
        , notDestructurable =
            destructureInExistingLets
        }

-}
destructureInExistingLets :
    OnlyFixInSeparateLetLeftFixOr
        DestructureInExistingLets
        separateLetUsed
destructureInExistingLets =
    Fix DestructureInExistingLets


{-| Destructure `(Pattern as argument)`.

    before o =
        case o of
            Opaque i ->
                f o i

    after ((Opaque i) as o) =
        f o i

For example

    fixByDestructuringInExistingLets
        { noExistingLets =
            destructureTheArgument
                { argumentAlsoUsedElsewhere =
                    destructureAs
                }
        }

-}
destructureAs :
    OnlyFixInSeparateLetLeftFixOr
        DestructureUsingAs
        separateLetUsed
destructureAs =
    Fix DestructureUsingAs


{-| Replace the argument with the pattern of the single case if no lets exist.

    before o =
        case o of
            Opaque i ->
                i

    after (Opaque i) =
        i

options:

  - | If the argument is a record field pattern or `as` destructured, uses your fix for the situation "only separate let fix left".
  - | If in `case` and `of` is not just an argument, uses your fix for the situation "only separate let fix left"
  - | `argumentAlsoUsedElsewhere`
      - [`destructureAs`](NoSinglePatternCase#destructureAs)
      - your fix for the situation "only separate let fix left"

For example

    destructureTheArgument
        { argumentAlsoUsedElsewhere =
            destructureAs
        }

-}
destructureTheArgument :
    { argumentAlsoUsedElsewhere :
        OnlyFixInSeparateLetLeftFixOr
            DestructureUsingAs
            separateLetUsed
    }
    ->
        OnlyFixInSeparateLetLeftFixOr
            (DestructureTheArgument separateLetUsed)
            separateLetUsed
destructureTheArgument =
    Fix << DestructureTheArgument


{-| Destructure `(Pattern as argument)`.

    before : Opaque -> Int
    before o =
        case o of
            Opaque i ->
                f o i

    after : Opaque -> Int
    after ((Opaque i) as o) =
        f o i

For example

    fixByDestructuringTheArgument
        { argumentAlsoUsedElsewhere =
            destructureAsInstead
        , ...
        }

-}
destructureAsInstead : ArgumentAlsoUsedElsewhere separateLetUsed
destructureAsInstead =
    UseAsInstead


{-| Destructure after existing let declarations.

    before o =
        let
            thing =
                ...
        in
        case o of
            Opaque i ->
                f i thing

    after o =
        let
            thing =
                ...

            (Opaque i) =
                o
        in
        f i thing

options:

  - | `noExistingLets`
      - [`destructureAs`](NoSinglePatternCase#destructureAs)
      - your fix for the situation "only separate let fix left"

For example

    fixByDestructuringTheArgument
        { argumentAlsoUsedElsewhere =
            destructureInExistingLetsInstead
                { noExistingLets = destructureAs }
        , ...
        }

-}
destructureInExistingLetsInstead :
    { noExistingLets :
        OnlyFixInSeparateLetLeftFixOr
            DestructureUsingAs
            separateLetUsed
    }
    -> ArgumentAlsoUsedElsewhere separateLetUsed
destructureInExistingLetsInstead =
    DestructureInExistingLetsInstead


{-| Destructure in a separately created `let` in this case.
-}
createSeparateLet : OnlyFixInSeparateLetLeftFixOr option CreateSeparateLet
createSeparateLet =
    OnlyFixInSeparateLetLeftFix


{-| Don't provide an automatic fix for this case.
-}
noFix : OnlyFixInSeparateLetLeftFixOr option DontCreateSeparateLet
noFix =
    OnlyFixInSeparateLetLeftFix



--


{-| Always fix by destructuring in the argument

    before : Opaque -> Int
    before o =
        case o of
            Opaque i ->
                i

    after : Opaque -> Int
    after (Opaque i) =
        i

even if an `as` pattern is required.

    before : Opaque -> Int
    before o =
        case o of
            Opaque i ->
                f o i

    after : Opaque -> Int
    after ((Opaque i) as o) =
        f o i

Equivalent to

    fixByDestructuringTheArgument
        { argumentAlsoUsedElsewhere = destructureAsInstead
        , notDestructurable = noFix
        }
        |> noFixOnNameClash

-}
alwaysFixInArgument : Config DontCreateSeparateLet
alwaysFixInArgument =
    fixByDestructuringTheArgument
        { argumentAlsoUsedElsewhere = destructureAsInstead
        , notDestructurable = noFix
        }
        |> noFixOnNameClash


{-| Always fix by destructuring in a `let` binding

    before o =
        let
            thing =
                ...
        in
        case o of
            Opaque i ->
                i

    after o =
        let
            thing =
                ...

            (Opaque i) =
                o
        in
        i

even if a separate `let` has to be created

    before o =
        case o of
            Opaque i ->
                i

    after o =
        let
            (Opaque i) =
                o
        in
        i

Equivalent to

    fixByDestructuringInExistingLets
        { noExistingLets = createSeparateLet }
        |> createSeparateLetOnNameClash

-}
alwaysFixInLet : Config CreateSeparateLet
alwaysFixInLet =
    fixByDestructuringInExistingLets
        { noExistingLets = createSeparateLet }
        |> createSeparateLetOnNameClash


{-| Check a TLD for single-pattern cases.
-}
checkDeclaration : Config separateLetUsed -> Declaration -> List (Error {})
checkDeclaration config d =
    case d of
        FunctionDeclaration { declaration } ->
            let
                { expression, arguments } =
                    Node.value declaration
            in
            checkExpression config
                { bindings =
                    List.concatMap (allBindingsInPattern (Node.value expression)) arguments
                        |> Dict.fromList
                , closestLetBlock = Nothing
                }
                expression

        _ ->
            []


type alias LocalContext =
    { bindings : Dict String Binding
    , closestLetBlock :
        Maybe
            { declarations : List LetDeclaration
            , expression : Node Expression
            , blockRange : Range
            }
    }


{-| Given a context, check an `Expression` for single-pattern cases.
-}
checkExpression : Config separateLetUsed -> LocalContext -> Node Expression -> List (Error {})
checkExpression config ({ bindings, closestLetBlock } as context) expressionNode =
    let
        checkExpressionHereWith { extraPatterns, newClosestLetBlock } =
            checkExpression config
                { bindings =
                    Dict.fromList extraPatterns
                        |> Dict.union bindings
                , closestLetBlock =
                    case newClosestLetBlock of
                        Just { expression, declarations } ->
                            Just
                                { blockRange = Node.range expressionNode
                                , expression = expression
                                , declarations = List.map Node.value declarations
                                }

                        Nothing ->
                            closestLetBlock
                }

        go extraPatterns =
            checkExpressionHereWith
                { extraPatterns = extraPatterns
                , newClosestLetBlock = Nothing
                }
    in
    case Node.value expressionNode of
        CaseExpression caseBlock ->
            case caseBlock.cases of
                [ ( singleCasePattern, Node _ singleCaseExpression ) ] ->
                    [ singlePatternCaseError config
                        { context = context
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
                            (\( p, e ) ->
                                -- Add pattern match bindings
                                go (allBindingsInPattern (Node.value e) p) e
                            )

        LetExpression letBlock ->
            let
                newVarsInLetDeclaration (Node _ letDeclaration) =
                    case letDeclaration of
                        LetDestructuring pattern _ ->
                            allBindingsInPattern (Node.value expressionNode) pattern

                        LetFunction fun ->
                            let
                                { name } =
                                    Node.value fun.declaration
                            in
                            [ ( Node.value name
                              , { patternNodeRange = Node.range name
                                , canDestructureAt = fun.signature == Nothing
                                , scope = Node.value expressionNode
                                }
                              )
                            ]

                checkExpressionInThisLetBlock newPatterns =
                    checkExpressionHereWith
                        { extraPatterns = List.concatMap newVarsInLetDeclaration letBlock.declarations ++ newPatterns
                        , newClosestLetBlock = Just letBlock
                        }

                checkLetDeclaration (Node _ letDeclaration) =
                    case letDeclaration of
                        LetFunction fun ->
                            let
                                { expression, arguments } =
                                    Node.value fun.declaration
                            in
                            checkExpressionInThisLetBlock
                                (arguments
                                    |> List.concatMap (allBindingsInPattern (Node.value expression))
                                )
                                expression

                        LetDestructuring _ expr ->
                            checkExpressionInThisLetBlock [] expr
            in
            checkExpressionInThisLetBlock [] letBlock.expression
                ++ List.concatMap checkLetDeclaration letBlock.declarations

        otherExpression ->
            subexpressions otherExpression
                |> List.concatMap (go [])


type alias SinglePatternCaseInfo =
    { context : LocalContext
    , expressionInCaseOf : Expression
    , singleCasePattern : Node Pattern
    , singleCaseExpression : Expression
    , caseRange : Range
    }


{-| An error for when a case expression only contains one case pattern. See [`Config`](NoSinglePatternCase#Config) for how fixes will be generated.
-}
singlePatternCaseError : Config separateLetUsed -> SinglePatternCaseInfo -> Error {}
singlePatternCaseError (Config fixKind onlySeparateLetFixLeft) information =
    let
        errorInfo =
            { message = "Single pattern case block."
            , details = [ "Single pattern case blocks typically are either unnecessary or overly verbose.  There's usually a more concise way to destructure, e.g. in a function argument, so consider refactoring." ]
            }

        { context, expressionInCaseOf, singleCaseExpression, caseRange, singleCasePattern } =
            information

        fix =
            case fixKind of
                FixByDestructuringInExistingLets { noExistingLets } ->
                    fixInExistingLets
                        { noExistingLets =
                            noExistingLets
                                |> onlyCreatingSeparateLetLeftFixOr
                                    (\(DestructureTheArgument { argumentAlsoUsedElsewhere }) ->
                                        case getDestructurablePattern expressionInCaseOf of
                                            Just varInCaseOf ->
                                                replaceVarPatternFixIfUsedOnce varInCaseOf
                                                    { usedOften =
                                                        argumentAlsoUsedElsewhere
                                                            |> onlyCreatingSeparateLetLeftFixOr
                                                                (\DestructureUsingAs ->
                                                                    destructureAsFix varInCaseOf
                                                                )
                                                    }

                                            Nothing ->
                                                onlySeparateLetFixLeftFix ()
                                    )
                        }

                FixByDestructuringTheArgument { argumentAlsoUsedElsewhere, notDestructurable } ->
                    case getDestructurablePattern expressionInCaseOf of
                        Just varInCaseOf ->
                            replaceVarPatternFixIfUsedOnce varInCaseOf
                                { usedOften =
                                    case argumentAlsoUsedElsewhere of
                                        UseAsInstead ->
                                            destructureAsFix varInCaseOf

                                        DestructureInExistingLetsInstead { noExistingLets } ->
                                            fixInExistingLets
                                                { noExistingLets =
                                                    noExistingLets
                                                        |> onlyCreatingSeparateLetLeftFixOr
                                                            (\DestructureUsingAs ->
                                                                destructureAsFix varInCaseOf
                                                            )
                                                }
                                }

                        Nothing ->
                            notDestructurable
                                |> onlyCreatingSeparateLetLeftFixOr
                                    (\DestructureInExistingLets ->
                                        fixInExistingLets
                                            { noExistingLets =
                                                onlySeparateLetFixLeftFix ()
                                            }
                                    )

        replaceVarPatternFixIfUsedOnce varInCaseOf { usedOften } =
            case usesIn varInCaseOf.scope varInCaseOf.name of
                1 ->
                    replaceVarPatternFix
                        { varRange = varInCaseOf.patternNodeRange
                        , varPatternScope = varInCaseOf.scope
                        }

                _ ->
                    usedOften

        getDestructurablePattern expression =
            case expression of
                FunctionOrValue [] varName ->
                    Dict.get varName context.bindings
                        |> Maybe.andThen
                            (\{ patternNodeRange, canDestructureAt, scope } ->
                                if canDestructureAt then
                                    Just
                                        { name = varName
                                        , patternNodeRange = patternNodeRange
                                        , scope = scope
                                        }

                                else
                                    Nothing
                            )

                _ ->
                    Nothing

        fixInExistingLets { noExistingLets } =
            case context.closestLetBlock of
                Just existingLetBlock ->
                    fixInLetBlock existingLetBlock

                Nothing ->
                    noExistingLets

        onlyCreatingSeparateLetLeftFixOr inFix options =
            case options of
                OnlyFixInSeparateLetLeftFix ->
                    onlySeparateLetFixLeftFix ()

                Fix option ->
                    inFix option

        onlySeparateLetFixLeftFix () =
            case onlySeparateLetFixLeft of
                CreateSeparateLet ->
                    createSeparateLetFix ()

                DontCreateSeparateLet ->
                    []

        replaceCaseWithExpressionAfterThePattern =
            Fix.replaceRangeBy caseRange
                (singleCaseExpression
                    |> prettyExpressionReplacing caseRange
                )

        replaceUselessCase { notUseless } pattern =
            -- Just use unit as "scope" here since all we care about is if any bindings are made
            if List.isEmpty <| allBindingsInPattern UnitExpr pattern then
                [ replaceCaseWithExpressionAfterThePattern ]

            else
                notUseless

        noNameClashIn scope =
            allBindingsInPattern singleCaseExpression singleCasePattern
                |> List.all
                    (Tuple.first >> usesIn scope >> (==) 1)

        onlySeparateLetFixLeftFixIfNameClashIn scope { noClash } =
            if noNameClashIn scope then
                noClash

            else
                onlySeparateLetFixLeftFix ()

        destructureAsFix { name, patternNodeRange, scope } =
            singleCasePattern
                |> replaceUselessCase
                    { notUseless =
                        onlySeparateLetFixLeftFixIfNameClashIn scope
                            { noClash =
                                [ Fix.replaceRangeBy patternNodeRange
                                    (asPattern (Node.value singleCasePattern) name
                                        |> prettyPrintPattern 120
                                    )
                                , replaceCaseWithExpressionAfterThePattern
                                ]
                            }
                    }

        fixInLetBlock letBlock =
            singleCasePattern
                |> replaceUselessCase
                    { notUseless =
                        onlySeparateLetFixLeftFixIfNameClashIn
                            (letExpr
                                letBlock.declarations
                                (letBlock.expression |> Node.value)
                            )
                            { noClash =
                                fixInLets letBlock singleCasePattern
                            }
                    }

        fixInLets letBlock pattern =
            [ Fix.replaceRangeBy letBlock.blockRange
                (letExpr
                    (letBlock.declarations
                        ++ [ letDestructuring
                                (parensAroundNamedPattern <| Node.value pattern)
                                expressionInCaseOf
                           ]
                    )
                    (let
                        replaceTheCase expression =
                            if Node.range expression == caseRange then
                                singleCaseExpression

                            else
                                expression
                                    |> Node.value
                                    |> mapSubexpressions
                                        (replaceTheCase >> Node emptyRange)
                     in
                     letBlock.expression |> replaceTheCase
                    )
                    |> prettyExpressionReplacing caseRange
                )
            ]

        replaceVarPatternFix { varRange, varPatternScope } =
            onlySeparateLetFixLeftFixIfNameClashIn varPatternScope
                { noClash =
                    [ Fix.replaceRangeBy varRange
                        (singleCasePattern
                            |> Node.value
                            |> parensAroundNamedPattern
                            |> prettyPrintPattern 120
                        )
                    , replaceCaseWithExpressionAfterThePattern
                    ]
                }

        createSeparateLetFix () =
            singleCasePattern
                |> replaceUselessCase
                    { notUseless =
                        [ Fix.replaceRangeBy caseRange
                            (letExpr
                                [ letDestructuring
                                    (parensAroundNamedPattern <| Node.value singleCasePattern)
                                    expressionInCaseOf
                                ]
                                singleCaseExpression
                                |> prettyExpressionReplacing caseRange
                            )
                        ]
                    }
    in
    Rule.errorWithFix errorInfo caseRange fix
