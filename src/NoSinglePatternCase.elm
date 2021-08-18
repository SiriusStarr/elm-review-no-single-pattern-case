module NoSinglePatternCase exposing
    ( rule
    , Config, fixInArgument, fixInLet
    , ifAsPatternRequired, ifArgumentNotDestructurable, ifNoLetExists
    , fail, fixInLetInstead, butIfNoLetExists, creatingNewLetIfNecessary, useAsPatternInstead, butIfAsPatternRequired, createNewLetInstead, fixInArgumentInstead, usingAsPatternIfNecessary
    , resolveNameClashWithSeparateLet
    )

{-|


# Rule

@docs rule


# Config

@docs Config, fixInArgument, fixInLet


## Customizing Config Behavior

@docs ifAsPatternRequired, ifArgumentNotDestructurable, ifNoLetExists


## Config Behavior Options

These functions are simply used by
[`ifAsPatternRequired`](#ifAsPatternRequired),
[`ifArgumentNotDestructurable`](#ifArgumentNotDestructurable), and
[`ifNoLetExists`](#ifNoLetExists) to customize behavior of the default configs
and are (hopefully) self-explanatory.

@docs fail, fixInLetInstead, butIfNoLetExists, creatingNewLetIfNecessary, useAsPatternInstead, butIfAsPatternRequired, createNewLetInstead, fixInArgumentInstead, usingAsPatternIfNecessary


## Resolving Name Clashes

@docs resolveNameClashWithSeparateLet

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
        , addParensToNamedPattern
        , allBindingsInPattern
        , countUsesIn
        , mapSubexpressions
        , prettyExpressionReplacing
        , prettyPrintPattern
        , subexpressions
        )


{-| Reports single-pattern case expressions, which may be written more concisely
or removed entirely.

    config =
        [ NoSinglePatternCase.rule NoSinglePatternCase.fixInArgument ]

See [`Config`](#Config) for configuration details.


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
rule : Config fixBy -> Rule
rule config =
    Rule.newModuleRuleSchema "NoSinglePatternCase" ()
        |> Rule.withSimpleDeclarationVisitor
            (Node.value >> checkDeclaration config)
        |> Rule.fromModuleRuleSchema


{-| Configure the rule, determining how automatic fixes are generated.

The default `Config`s [`fixInArgument`](#fixInArgument) and
[`fixInLet`](#fixInLet) should be used as reasonable defaults, with more
customization detailed in those sections.

All methods of fixing can generate name clashes, as the scope of the name will
necessarily be larger. By default, such name clashes will not be automatically
fixed (though `elm-review` errors will still be generated so they can be
manually resolved). They can, however, be automatically fixed by creating a
separate `let` block just to destructure the pattern in. Use
[`resolveNameClashWithSeparateLet`](#resolveNameClashWithSeparateLet) if this
behavior is desired.

-}
type Config fixBy
    = Config { fixBy : FixBy, useSeparateLetForNameClash : Bool }


{-| Phantom type for `Config fixBy`.
-}
type FixInArgument
    = FixInArgument Never


{-| Phantom type for `Config fixBy`.
-}
type FixInLet
    = FixInLet Never


{-| Always fix by destructuring in the argument. This will use `as` patterns if
necessary. If the argument cannot be destructured in, no fix will be generated.

For example:

    f1 o =
        case o of
            Opaque i ->
                i

    f2 o =
        let
            x =
                someFunc o
        in
        case o of
            Opaque i ->
                i + x

    f3 { recordField } =
        case recordField of
            Opaque i ->
                i

will be fixed to:

    f1 (Opaque i) =
        i

    f2 ((Opaque i) as o) =
        let
            x =
                someFunc o
        in
        i + x

    f3 { recordField } =
        case recordField of
            Opaque i ->
                i

Use [`ifAsPatternRequired`](#ifAsPatternRequired) and
[`ifArgumentNotDestructurable`](#ifArgumentNotDestructurable) to customize the
behavior in either of these cases.

-}
fixInArgument : Config FixInArgument
fixInArgument =
    Config
        { fixBy =
            DestructureInArgument
                { asPatternRequired = Fix UseAsPattern
                , notDestructurable = Fallback Fail
                }
        , useSeparateLetForNameClash = False
        }


{-| Always fix by destructuring in a `let` block, creating a new one if none
exists.
For example:

    f1 o =
        case o of
            Opaque i ->
                i

    f2 o =
        let
            x =
                someFunc o
        in
        case o of
            Opaque i ->
                i + x

will be fixed to:

    f1 o =
        let
            (Opaque i) =
                o
        in
        i

    f2 ((Opaque i) as o) =
        let
            x =
                someFunc o

            (Opaque i) =
                o
        in
        i + x

Use [`ifNoLetExists`](#ifNoLetExists) to customize the behavior in the case
where no `let` block exists.

-}
fixInLet : Config FixInLet
fixInLet =
    Config
        { fixBy =
            DestructureInLet { ifNoLetExists = Fix CreateNewLet }
        , useSeparateLetForNameClash = False
        }


{-| Choose different behavior if an `as` pattern would be required to
destructure in the argument, e.g.

    f o =
        let
            x =
                someFunc o
        in
        case o of
            Opaque i ->
                i + x

Available options are [`fixInLetInstead`](#fixInLetInstead) or [`fail`](#fail),
e.g.

    c1 =
        fixInArgument
            |> ifAsPatternRequired fail

    c2 =
        fixInArgument
            |> ifAsPatternRequired
                (fixInLetInstead creatingNewLetIfNecessary)

    c3 =
        fixInArgument
            |> ifAsPatternRequired
                (fixInLetInstead
                    |> butIfNoLetExists fail
                )

    c4 =
        fixInArgument
            |> ifAsPatternRequired
                (fixInLetInstead
                    |> butIfNoLetExists useAsPatternInstead
                )

-}
ifAsPatternRequired : FixOrFallback (FallbackToExistingLet (FixOrFallback UseAsPattern Fail)) Fail -> Config FixInArgument -> Config FixInArgument
ifAsPatternRequired fixOrFallback (Config r) =
    case r.fixBy of
        DestructureInArgument c ->
            Config { r | fixBy = DestructureInArgument { c | asPatternRequired = Fallback fixOrFallback } }

        DestructureInLet _ ->
            Config r


{-| Specify what to do if the argument cannot be destructured at, e.g.

    f { recordField } =
        case recordField of
            Opaque i ->
                i

Available options are [`fixInLetInstead`](#fixInLetInstead) or [`fail`](#fail)
(this latter is the default), e.g.

    c1 =
        fixInArgument
            |> ifArgumentNotDestructurable
                (fixInLetInstead creatingNewLetIfNecessary)

    c2 =
        fixInArgument
            |> ifArgumentNotDestructurable
                (fixInLetInstead
                    |> butIfNoLetExists fail
                )

-}
ifArgumentNotDestructurable : FixOrFallback (FallbackToExistingLet (FixOrFallback Fail Fail)) Fail -> Config FixInArgument -> Config FixInArgument
ifArgumentNotDestructurable fixOrFallback (Config r) =
    case r.fixBy of
        DestructureInArgument c ->
            Config { r | fixBy = DestructureInArgument { c | notDestructurable = fixOrFallback } }

        DestructureInLet _ ->
            Config r


{-| Specify what to do it no `let` block exists in scope, instead of creating a
new one.

Available options are [`fixInArgumentInstead`](#fixInArgumentInstead) or
[`fail`](#fail), e.g.

    c1 =
        fixInLet
            |> ifNoLetExists fail

    c2 =
        fixInLet
            |> ifNoLetExists
                (fixInArgumentInstead usingAsPatternIfNecessary)

    c3 =
        fixInLet
            |> ifNoLetExists
                (fixInArgumentInstead
                    |> butIfAsPatternRequired fail
                )

    c4 =
        fixInLet
            |> ifNoLetExists
                (fixInArgumentInstead
                    |> butIfAsPatternRequired createNewLetInstead
                )

-}
ifNoLetExists : FixOrFallback FallbackToArgument Fail -> Config FixInLet -> Config FixInLet
ifNoLetExists fixOrFallback (Config r) =
    Config { r | fixBy = DestructureInLet { ifNoLetExists = Fallback fixOrFallback } }


{-| Fallback to destructuring in a `let` block instead of the argument.
-}
fixInLetInstead : FixOrFallback CreateNewLet a -> FixOrFallback (FallbackToExistingLet a) Fail
fixInLetInstead f =
    Fix <| FallbackToExistingLet { ifNoLetExists = f }


{-| Choose to create a new `let` block to destructure in if none exists in
scope.
-}
creatingNewLetIfNecessary : FixOrFallback CreateNewLet a
creatingNewLetIfNecessary =
    Fix CreateNewLet


{-| If no `let` block exists to destructure in, choose some other behavior
instead.
-}
butIfNoLetExists : a -> (FixOrFallback CreateNewLet a -> FixOrFallback (FallbackToExistingLet a) Fail) -> FixOrFallback (FallbackToExistingLet a) Fail
butIfNoLetExists fallback f =
    f <| Fallback fallback


{-| Choose to use an `as` pattern to destructure in the argument if necessary.
-}
useAsPatternInstead : FixOrFallback UseAsPattern a
useAsPatternInstead =
    Fix UseAsPattern


{-| Fallback to destructuring in the argument instead of a `let` block.
-}
fixInArgumentInstead : FixOrFallback UseAsPattern (FixOrFallback CreateNewLet Fail) -> FixOrFallback FallbackToArgument Fail
fixInArgumentInstead f =
    Fix <| FallbackToArgument { asPatternRequired = f }


{-| Choose to use an `as` pattern to destructure in the argument if necessary.
-}
usingAsPatternIfNecessary : FixOrFallback UseAsPattern (FixOrFallback CreateNewLet Fail)
usingAsPatternIfNecessary =
    Fix UseAsPattern


{-| If an `as` pattern would be necessary, choose some other behavior instead.
-}
butIfAsPatternRequired : FixOrFallback CreateNewLet Fail -> (FixOrFallback UseAsPattern (FixOrFallback CreateNewLet Fail) -> FixOrFallback FallbackToArgument Fail) -> FixOrFallback FallbackToArgument Fail
butIfAsPatternRequired fallback f =
    f <| Fallback fallback


{-| Choose to create a `let` block when none exists, instead of failing.
-}
createNewLetInstead : FixOrFallback CreateNewLet Fail
createNewLetInstead =
    Fix CreateNewLet


{-| Choose to fail at generating a fix.
-}
fail : FixOrFallback fix Fail
fail =
    Fallback Fail


{-| Change the default behavior to instead resolve name clashes by creating a
separate `let` block. For example:

    f o =
        let
            j i =
                i + 1
        in
        case o of
            Opaque i ->
                j i

instead of generating an error with no automatic fix, will be automatically
fixed to:

    f o =
        let
            j i =
                i + 1
        in
        let
            (Opaque i) =
                o
        in
        j i

-}
resolveNameClashWithSeparateLet : Config fixBy -> Config fixBy
resolveNameClashWithSeparateLet (Config r) =
    Config { r | useSeparateLetForNameClash = True }


{-| Specify how to automatically fix single-pattern cases.

  - `DestructureInLet` -- Destructure in a `let` block.
  - `DestructureInArgument` -- Destructure in the argument.

Note that `(FixOrFallback Fail Fail)` is necessary to allow `fail` to be used in
builders.

-}
type FixBy
    = DestructureInLet
        { ifNoLetExists :
            FixOrFallback CreateNewLet (FixOrFallback FallbackToArgument Fail)
        }
    | DestructureInArgument
        { asPatternRequired :
            FixOrFallback UseAsPattern (FixOrFallback (FallbackToExistingLet (FixOrFallback UseAsPattern Fail)) Fail)
        , notDestructurable :
            FixOrFallback (FallbackToExistingLet (FixOrFallback Fail Fail)) Fail
        }


{-| Offer a choice between a fix or a fallback.
-}
type FixOrFallback fix fallback
    = Fix fix
    | Fallback fallback


{-| Fallback to destructuring in the argument, if choosing not to create a `let`
block.
-}
type FallbackToArgument
    = FallbackToArgument { asPatternRequired : FixOrFallback UseAsPattern (FixOrFallback CreateNewLet Fail) }


{-| Fallback to using a `let` block if destructuring in the argument has failed
(or we've chosen not to). If we could instead use an `as` pattern, then
`canUseAsPattern` should be `(FixOrFallback UseAsPattern Fail)`. Otherwise,
only failure is an option, so use `(FixOrFallback Fail Fail)`.
-}
type FallbackToExistingLet canUseAsPattern
    = FallbackToExistingLet { ifNoLetExists : FixOrFallback CreateNewLet canUseAsPattern }


{-| Use an `as` pattern when one is necessary.
-}
type UseAsPattern
    = UseAsPattern


{-| Create a new `let` block when none exists.
-}
type CreateNewLet
    = CreateNewLet


{-| Give up on generating a fix.
-}
type Fail
    = Fail


{-| Check a TLD for single-pattern cases.
-}
checkDeclaration : Config fixBy -> Declaration -> List (Error {})
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
checkExpression : Config fixBy -> LocalContext -> Node Expression -> List (Error {})
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
            case countUsesIn varInCaseOf.scope varInCaseOf.name of
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
                    (Tuple.first >> countUsesIn scope >> (==) 1)

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
                                (addParensToNamedPattern <| Node.value pattern)
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
                            |> addParensToNamedPattern
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
                                    (addParensToNamedPattern <| Node.value singleCasePattern)
                                    expressionInCaseOf
                                ]
                                singleCaseExpression
                                |> prettyExpressionReplacing caseRange
                            )
                        ]
                    }
    in
    Rule.errorWithFix errorInfo caseRange fix
