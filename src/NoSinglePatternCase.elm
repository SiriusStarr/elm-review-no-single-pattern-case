module NoSinglePatternCase exposing
    ( rule
    , Config, fixInArgument, fixInLet
    , ifAsPatternRequired, ifCannotDestructureAtArgument, ifNoLetExists
    , fail, createNewLet, useAsPattern, fixInArgumentInstead, andIfAsPatternRequired, andIfCannotDestructureAtArgument, fixInLetInstead, andIfNoLetExists
    , FixInArgument, FixInLet, UseArgInstead, UseLetInstead, CreateNewLet, Fail, UseAsPattern, UseAsPatternOrFailOr, CreateNewLetOr, UseArgOrCreateNewLetOrFail, UseLetOr, UseLetOrFail, UseAsPatternOrLetsOrFail, Either
    )

{-|


# Rule

@docs rule


# Config

@docs Config, fixInArgument, fixInLet


## Customizing Config Behavior

@docs ifAsPatternRequired, ifCannotDestructureAtArgument, ifNoLetExists


## Config Behavior Options

These functions are simply used by
[`ifAsPatternRequired`](#ifAsPatternRequired),
[`ifCannotDestructureAtArgument`](#ifCannotDestructureAtArgument), and
[`ifNoLetExists`](#ifNoLetExists) to customize behavior of the default configs.
Look at the examples in those to understand how to use them.

@docs fail, createNewLet, useAsPattern, fixInArgumentInstead, andIfAsPatternRequired, andIfCannotDestructureAtArgument, fixInLetInstead, andIfNoLetExists


## Types

You shouldn't need to worry about these types; they are exported solely for the
sake of annotation, should it be necessary.

@docs FixInArgument, FixInLet, UseArgInstead, UseLetInstead, CreateNewLet, Fail, UseAsPattern, UseAsPatternOrFailOr, CreateNewLetOr, UseArgOrCreateNewLetOrFail, UseLetOr, UseLetOrFail, UseAsPatternOrLetsOrFail, Either

-}

import Dict exposing (Dict)
import Elm.CodeGen exposing (letDestructuring, letExpr)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), LetBlock, LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range, emptyRange)
import Maybe.Extra as MaybeX
import Review.Fix as Fix exposing (Fix)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)
import Util
    exposing
        ( Binding
        , addParensToNamedPattern
        , allBindingsInPattern
        , allBindingsUsedInExpression
        , countUsesIn
        , either
        , mapSubexpressions
        , nameUsedOutsideExpr
        , prettyExpressionReplacing
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
    Rule.newModuleRuleSchemaUsingContextCreator "NoSinglePatternCase" initialContext
        |> Rule.withDeclarationEnterVisitor
            (checkDeclaration config)
        |> Rule.fromModuleRuleSchema


{-| Module context for the rule.
-}
type alias ModuleContext =
    { extractSourceCode : Range -> String }


initialContext : Rule.ContextCreator () ModuleContext
initialContext =
    Rule.initContextCreator
        (\extractSourceCode () -> { extractSourceCode = extractSourceCode })
        |> Rule.withSourceCodeExtractor


{-| Configure the rule, determining how automatic fixes are generated.

The default `Config`s [`fixInArgument`](#fixInArgument) and
[`fixInLet`](#fixInLet) should be used as reasonable defaults, with more
customization detailed in those sections.

-}
type Config fixBy
    = Config { fixBy : FixBy }


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
[`ifCannotDestructureAtArgument`](#ifCannotDestructureAtArgument) to customize
the behavior in either of these cases.

-}
fixInArgument : Config FixInArgument
fixInArgument =
    Config
        { fixBy =
            DestructureInArgument
                { ifAsPatternNeeded = useAsPattern
                , ifCannotDestructure = fail
                }
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
where no `let` block exists within the scope the pattern is valid in. To
clarify, the following counts as no `let` existing:

    unpack : Opaque -> Int
    unpack o =
        let
            foo =
                bar
        in
        (\a ->
            case a of
                Opaque i ->
                    i
        )
            o

as the name `a` is not in scope in the extant `let` block.

[`ifNoLetExists`](#ifNoLetExists) also handles the case where the closest `let`
block would result in a name clash. To clarify, the following counts as no
`let` existing:

    unpack : Opaque -> Int
    unpack o =
        let
            foo =
                (\i -> i + 1) 0
        in
        case o of
            Opaque i ->
                i

as `i` cannot be unpacked in the `let` block, as doing so would cause a name
clash with the `i` in `foo`.

-}
fixInLet : Config FixInLet
fixInLet =
    Config
        { fixBy =
            DestructureInLet { ifNoLetBlock = createNewLet }
        }


{-| Specify what to do if an `as` pattern would be required to destructure in
the argument, e.g.

    f o =
        let
            x =
                someFunc o
        in
        case o of
            Opaque i ->
                i + x

Available options are [`useAsPattern`](#useAsPattern) (this is the default),
[`fixInLetInstead`](#fixInLetInstead) or [`fail`](#fail),
e.g.

    c1 =
        fixInArgument
            |> ifAsPatternRequired fail

    c2 =
        fixInArgument
            |> ifAsPatternRequired useAsPattern

    c3 =
        fixInArgument
            |> ifAsPatternRequired
                (fixInLetInstead
                    |> andIfNoLetExists createNewLet
                )

    c4 =
        fixInArgument
            |> ifAsPatternRequired
                (fixInLetInstead
                    |> andIfNoLetExists useAsPattern
                )

-}
ifAsPatternRequired : UseAsPatternOrLetsOrFail -> Config FixInArgument -> Config FixInArgument
ifAsPatternRequired e (Config r) =
    case r.fixBy of
        DestructureInArgument c ->
            Config { r | fixBy = DestructureInArgument { c | ifAsPatternNeeded = e } }

        DestructureInLet _ ->
            Config r


{-| Specify what to do if the argument cannot be destructured at, either due to
it being a record field, e.g.

    f { recordField } =
        case recordField of
            Opaque i ->
                i

or a more complex `case` expression, e.g.

    f a =
        case foo <| bar a of
            Opaque i ->
                i

or due to a name clash that would be caused by the increase in scope, e.g.

    unpack : Opaque -> Int
    unpack o =
        let
            foo =
                (\i -> i + 1) 0
        in
        case o of
            Opaque i ->
                i

Available options are [`fixInLetInstead`](#fixInLetInstead) or [`fail`](#fail)
(this is the default).

    c1 =
        fixInArgument
            |> ifCannotDestructureAtArgument fail

    c2 =
        fixInArgument
            |> ifCannotDestructureAtArgument
                (fixInLetInstead
                    |> andIfNoLetExists fail
                )

    c3 =
        fixInArgument
            |> ifCannotDestructureAtArgument
                (fixInLetInstead
                    |> andIfNoLetExists createNewLet
                )

-}
ifCannotDestructureAtArgument : UseLetOrFail -> Config FixInArgument -> Config FixInArgument
ifCannotDestructureAtArgument e (Config r) =
    case r.fixBy of
        DestructureInArgument c ->
            Config { r | fixBy = DestructureInArgument { c | ifCannotDestructure = e } }

        DestructureInLet _ ->
            Config r


{-| Specify what to do it no `let` block exists in scope, instead of creating a
new one.

Available options are [`fixInArgumentInstead`](#fixInArgumentInstead),
[`createNewLet`](#createNewLet) (this is the default), or [`fail`](#fail). Note
that [`andIfAsPatternRequired`](#andIfAsPatternRequired) and
[`andIfCannotDestructureAtArgument`](#andIfCannotDestructureAtArgument) must appear
in that order after [`fixInArgumentInstead`](#fixInArgumentInstead).

    c1 =
        fixInLet
            |> ifNoLetExists fail

    c2 =
        fixInLet
            -- This is the default
            |> ifNoLetExists createNewLet

    c3 =
        fixInLet
            |> ifNoLetExists
                (fixInArgumentInstead
                    |> andIfAsPatternRequired useAsPattern
                    |> andIfCannotDestructureAtArgument fail
                )

-}
ifNoLetExists : UseArgOrCreateNewLetOrFail -> Config FixInLet -> Config FixInLet
ifNoLetExists e (Config r) =
    Config { r | fixBy = DestructureInLet { ifNoLetBlock = e } }


{-| Fallback to destructuring in a `let` block instead of the argument.
-}
fixInLetInstead : a -> UseLetOr a b
fixInLetInstead f =
    UseLetInstead { ifNoLetBlock = f }
        |> Util.A
        |> Util.A


{-| If no `let` block exists to destructure in, choose some other behavior
instead.
-}
andIfNoLetExists : a -> (a -> UseLetOr a b) -> UseLetOr a b
andIfNoLetExists fallback f =
    f <| fallback


{-| Fallback to destructuring in the argument instead of a `let` block.

Note that [`andIfAsPatternRequired`](#andIfAsPatternRequired) and
[`andIfCannotDestructureAtArgument`](#andIfCannotDestructureAtArgument) must appear
in that order, e.g.

    c =
        fixInLet
            |> ifNoLetExists
                (fixInArgumentInstead
                    |> andIfAsPatternRequired useAsPattern
                    |> andIfCannotDestructureAtArgument fail
                )

-}
fixInArgumentInstead : UseAsPatternOrFailOr CreateNewLet -> CreateNewLetOr Fail -> UseArgOrCreateNewLetOrFail
fixInArgumentInstead ifAsPatternNeeded ifCannotDestructure =
    UseArgInstead
        { ifAsPatternNeeded = ifAsPatternNeeded
        , ifCannotDestructure = ifCannotDestructure
        }
        |> Util.B
        |> Util.A


{-| Specify what to do if an `as` pattern would be necessary.

Available options are [`useAsPattern`](#useAsPattern),
[`createNewLet`](#createNewLet) or [`fail`](#fail)

    c1 =
        fixInLet
            |> ifNoLetExists
                (fixInArgumentInstead
                    |> andIfAsPatternRequired useAsPattern
                    |> andIfCannotDestructureAtArgument fail
                )

    c2 =
        fixInLet
            |> ifNoLetExists
                (fixInArgumentInstead
                    |> andIfAsPatternRequired createNewLet
                    |> andIfCannotDestructureAtArgument fail
                )

    c3 =
        fixInLet
            |> ifNoLetExists
                (fixInArgumentInstead
                    |> andIfAsPatternRequired fail
                    |> andIfCannotDestructureAtArgument fail
                )

-}
andIfAsPatternRequired : UseAsPatternOrFailOr CreateNewLet -> (UseAsPatternOrFailOr CreateNewLet -> CreateNewLetOr Fail -> UseArgOrCreateNewLetOrFail) -> (CreateNewLetOr Fail -> UseArgOrCreateNewLetOrFail)
andIfAsPatternRequired ifAsPatternNeeded f =
    f ifAsPatternNeeded


{-| Choose to use an `as` pattern to destructure in the argument if necessary.
-}
useAsPattern : UseAsPatternOrFailOr or
useAsPattern =
    Util.A <| Util.B UseAsPattern


{-| Specify what to do if the argument cannot be destructured at, e.g.

    f { recordField } =
        case recordField of
            Opaque i ->
                i

Available options are [`createNewLet`](#createNewLet) or [`fail`](#fail)

    c1 =
        fixInLet
            |> ifNoLetExists
                (fixInArgumentInstead
                    |> andIfAsPatternRequired fail
                    |> andIfCannotDestructureAtArgument createNewLet
                )

    c2 =
        fixInLet
            |> ifNoLetExists
                (fixInArgumentInstead
                    |> andIfAsPatternRequired fail
                    |> andIfCannotDestructureAtArgument fail
                )

-}
andIfCannotDestructureAtArgument : CreateNewLetOr Fail -> (CreateNewLetOr Fail -> UseArgOrCreateNewLetOrFail) -> UseArgOrCreateNewLetOrFail
andIfCannotDestructureAtArgument ifCannotDestructure f =
    f ifCannotDestructure


{-| Choose to create a `let` block when none exists.
-}
createNewLet : CreateNewLetOr or
createNewLet =
    Util.A <| Util.A CreateNewLet


{-| Choose to fail at generating a fix.
-}
fail : Either or Fail
fail =
    Util.B Fail


{-| Specify how to automatically fix single-pattern cases.

  - `DestructureInLet` -- Destructure in a `let` block.
  - `DestructureInArgument` -- Destructure in the argument.

-}
type FixBy
    = DestructureInLet { ifNoLetBlock : UseArgOrCreateNewLetOrFail }
    | DestructureInArgument
        { ifAsPatternNeeded : UseAsPatternOrLetsOrFail
        , ifCannotDestructure : UseLetOrFail
        }


{-| At this point, an `as` pattern could be used, or we could use a `let` block.
-}
type alias UseAsPatternOrLetsOrFail =
    UseAsPatternOrFailOr (UseLetInstead (UseAsPatternOrFailOr CreateNewLet))


{-| At this point, the only option is to use a `let` block.
-}
type alias UseLetOrFail =
    UseLetOr (CreateNewLetOr Fail) Fail


{-| At this point, the argument could be used or a new `let` block created.
block.
-}
type alias UseArgOrCreateNewLetOrFail =
    Either (Either CreateNewLet UseArgInstead) Fail


{-| At this point, an `as` pattern could be used or some other option.
-}
type alias UseAsPatternOrFailOr or =
    Either (Either or UseAsPattern) Fail


{-| At this point, a `let` could be used or either of two other options.
-}
type alias UseLetOr or1 or2 =
    Either (Either (UseLetInstead or1) or2) Fail


{-| At this point, a new `let` could be created or some other option.
-}
type alias CreateNewLetOr or =
    Either (Either CreateNewLet or) Fail


{-| Choose to fail at generating a fix.
-}
type Fail
    = Fail


{-| Choose to create a new `let` block to destructure in.
-}
type CreateNewLet
    = CreateNewLet


{-| Choose to use an `as` pattern.
-}
type UseAsPattern
    = UseAsPattern


{-| Fallback to destructuring in the argument, if choosing not to create a `let`
block.
-}
type UseArgInstead
    = UseArgInstead
        { ifAsPatternNeeded : UseAsPatternOrFailOr CreateNewLet
        , ifCannotDestructure : CreateNewLetOr Fail
        }


{-| Fallback to using a `let` block if destructuring in the argument has failed
(or we've chosen not to).
-}
type UseLetInstead noValidLetOptions
    = UseLetInstead { ifNoLetBlock : noValidLetOptions }


{-| Offer a choice between two options.
-}
type alias Either a b =
    Util.Either a b


{-| Check a TLD for single-pattern cases.
-}
checkDeclaration : Config fixBy -> Node Declaration -> ModuleContext -> ( List (Error {}), ModuleContext )
checkDeclaration config d context =
    ( case Node.value d of
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
                , newBindingsSinceLastLet = Set.empty
                , extractSourceCode = context.extractSourceCode
                }
                expression

        _ ->
            []
    , context
    )


type alias LocalContext =
    { bindings : Dict String Binding
    , closestLetBlock :
        Maybe
            { expression : Node Expression
            , letBlock : LetBlock
            }
    , newBindingsSinceLastLet : Set String
    , extractSourceCode : Range -> String
    }


{-| Given a context, check an `Expression` for single-pattern cases.
-}
checkExpression : Config fixBy -> LocalContext -> Node Expression -> List (Error {})
checkExpression config ({ bindings } as context) expressionNode =
    let
        go : Maybe LetBlock -> List ( String, Binding ) -> Node Expression -> List (Error {})
        go newLet extraPatterns =
            case ( extraPatterns, newLet ) of
                ( [], Nothing ) ->
                    checkExpression config context

                ( bs, Just l ) ->
                    checkExpression config
                        { context
                            | bindings =
                                Dict.fromList bs
                                    |> Dict.union bindings
                            , closestLetBlock =
                                Just <| { letBlock = l, expression = expressionNode }
                            , newBindingsSinceLastLet = Set.empty
                        }

                ( bs, Nothing ) ->
                    checkExpression config
                        { context
                            | bindings =
                                Dict.fromList bs
                                    |> Dict.union bindings
                            , newBindingsSinceLastLet =
                                List.map Tuple.first bs
                                    |> Set.fromList
                                    |> Set.union context.newBindingsSinceLastLet
                        }
    in
    case Node.value expressionNode of
        CaseExpression caseBlock ->
            case caseBlock.cases of
                [ ( singleCasePattern, singleCaseExpression ) ] ->
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
                                go Nothing (allBindingsInPattern (Node.value e) p) e
                            )

        LetExpression lB ->
            let
                bindingsInDecl : Node LetDeclaration -> List ( String, Binding )
                bindingsInDecl d =
                    case Node.value d of
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

                letBindings : List ( String, Binding )
                letBindings =
                    List.concatMap bindingsInDecl lB.declarations

                checkDecl : Node LetDeclaration -> List (Error {})
                checkDecl d =
                    case Node.value d of
                        LetFunction fun ->
                            let
                                { expression, arguments } =
                                    Node.value fun.declaration
                            in
                            List.concatMap (allBindingsInPattern (Node.value expression)) arguments
                                |> (\bs ->
                                        go (Just lB) (bs ++ letBindings) expression
                                   )

                        LetDestructuring _ expr ->
                            go (Just lB) letBindings expr
            in
            go (Just lB) letBindings lB.expression
                ++ List.concatMap checkDecl lB.declarations

        LambdaExpression { args, expression } ->
            -- Add arg bindings
            List.concatMap (allBindingsInPattern <| Node.value expression) args
                |> (\bs ->
                        go Nothing bs expression
                   )

        expressionWithoutBindings ->
            subexpressions expressionWithoutBindings
                |> List.concatMap (go Nothing [])


type alias SinglePatternCaseInfo =
    { context : LocalContext
    , expressionInCaseOf : Expression
    , singleCasePattern : Node Pattern
    , singleCaseExpression : Node Expression
    , caseRange : Range
    }


{-| An error for when a case expression only contains one case pattern. See [`Config`](NoSinglePatternCase#Config) for how fixes will be generated.
-}
singlePatternCaseError : Config fixBy -> SinglePatternCaseInfo -> Error {}
singlePatternCaseError config info =
    (-- Check for useless cases.  This is also caught by `elm-review-simplify`,
     -- but we'll handle it in case they don't have that in their review config.
     -- Just use unit as "scope" here since all we care about is if any bindings are made
     if
        allBindingsInPattern UnitExpr info.singleCasePattern
            |> List.all ((==) 0 << countUsesIn (Node.value info.singleCaseExpression) << Tuple.first)
     then
        [ replaceCaseBlockWithExpression info ]

     else
        makeFix config info
            |> Maybe.withDefault []
    )
        |> Rule.errorWithFix
            { message = "Single pattern case block."
            , details = [ "Single pattern case blocks typically are either unnecessary or overly verbose.  There's usually a more concise way to destructure, e.g. in a function argument, so consider refactoring." ]
            }
            info.caseRange


{-| Given config and info about a single pattern case, try to create a fix per
the config or fail.
-}
makeFix : Config fixBy -> SinglePatternCaseInfo -> Maybe (List Fix)
makeFix (Config { fixBy }) ({ context, expressionInCaseOf, singleCaseExpression, caseRange, singleCasePattern } as info) =
    let
        destructureInLet : (Either a b -> Maybe (List Fix)) -> Either (Either a b) Fail -> Maybe (List Fix)
        destructureInLet fallback ifNoLetBlock =
            case getValidLetBlock context expressionInCaseOf ( singleCasePattern, singleCaseExpression ) of
                Just existingLetBlock ->
                    [ moveCasePatternToLetBlock ( expressionInCaseOf, caseRange ) ( singleCasePattern, singleCaseExpression ) existingLetBlock ]
                        |> Just

                Nothing ->
                    either fallback orFail ifNoLetBlock

        useNewLet : CreateNewLet -> Maybe (List Fix)
        useNewLet CreateNewLet =
            Just <| fixInNewLet ( expressionInCaseOf, caseRange ) ( singleCasePattern, singleCaseExpression )

        fallbackToArg : UseArgInstead -> Maybe (List Fix)
        fallbackToArg (UseArgInstead { ifAsPatternNeeded, ifCannotDestructure }) =
            destructureInArg (always useNewLet) useNewLet ifAsPatternNeeded ifCannotDestructure

        orFail : Fail -> Maybe (List Fix)
        orFail Fail =
            Nothing

        fallbackToLet : (a -> Maybe (List Fix)) -> (b -> Maybe (List Fix)) -> UseLetInstead (Either (Either a b) Fail) -> Maybe (List Fix)
        fallbackToLet fA fB (UseLetInstead { ifNoLetBlock }) =
            destructureInLet (either fA fB) ifNoLetBlock

        orUseAsPattern : ( String, Binding ) -> UseAsPattern -> Maybe (List Fix)
        orUseAsPattern ( name, binding ) UseAsPattern =
            moveCasePatternToBinding info binding (Just name)
                |> Just

        destructureInArg : (( String, Binding ) -> asFallback -> Maybe (List Fix)) -> (destructureFallback -> Maybe (List Fix)) -> Either (Either asFallback UseAsPattern) Fail -> Either (Either destructureFallback Fail) Fail -> Maybe (List Fix)
        destructureInArg asFallback destructureFallback ifAsPatternNeeded ifCannotDestructure =
            case getValidPatternBinding context expressionInCaseOf ( singleCasePattern, singleCaseExpression ) of
                Just { name, binding, requiresAsPattern } ->
                    if requiresAsPattern then
                        either (either (asFallback ( name, binding )) (orUseAsPattern ( name, binding ))) orFail ifAsPatternNeeded

                    else
                        moveCasePatternToBinding info binding Nothing
                            |> Just

                Nothing ->
                    either (either destructureFallback orFail) orFail ifCannotDestructure
    in
    case fixBy of
        DestructureInLet { ifNoLetBlock } ->
            destructureInLet (either useNewLet fallbackToArg) ifNoLetBlock

        DestructureInArgument { ifAsPatternNeeded, ifCannotDestructure } ->
            destructureInArg (\b -> fallbackToLet useNewLet (orUseAsPattern b)) (fallbackToLet useNewLet orFail) ifAsPatternNeeded ifCannotDestructure


{-| Given the full range of a `case` block, a binding to destructure at, maybe a
name for an `as` pattern, and a single case pattern and expression, generate
fixes destructuring in the binding. This function does not check if this is
possible, and should only be called after `getValidPatternBinding`.
-}
moveCasePatternToBinding : SinglePatternCaseInfo -> Binding -> Maybe String -> List Fix
moveCasePatternToBinding ({ context, singleCasePattern } as info) { patternNodeRange } asName =
    Node.range singleCasePattern
        |> context.extractSourceCode
        |> (\p -> MaybeX.unwrap p (\n -> String.concat [ "(", p, ") as ", n ]) asName)
        |> (\p -> "(" ++ p ++ ")")
        |> Fix.replaceRangeBy patternNodeRange
        |> (\f -> [ f, replaceCaseBlockWithExpression info ])


{-| Given context, an expression in a `case...of`, and a single case pattern and
single case expression, return the binding that the pattern could be
destructured at, if one exists, and whether or not an `as` pattern is required
to do so. This function also checks for possible name clashes.
-}
getValidPatternBinding : LocalContext -> Expression -> ( Node Pattern, Node Expression ) -> Maybe { requiresAsPattern : Bool, name : String, binding : Binding }
getValidPatternBinding context caseExpr ( replacePattern, replaceScope ) =
    getDestructurableBinding context caseExpr
        |> Maybe.andThen
            (\( name, { scope } as b ) ->
                if
                    allBindingsInPattern (Node.value replaceScope) replacePattern
                        |> List.any (\( n, _ ) -> nameUsedOutsideExpr n (Node.value replaceScope) scope)
                then
                    -- Cannot move the pattern if it would cause name clash
                    Nothing

                else
                    Just
                        { requiresAsPattern = nameUsedOutsideExpr name caseExpr scope
                        , name = name
                        , binding = b
                        }
            )


{-| Given the expression in `case ... of` and the range of the entire `case`,
the single case pattern, the single case expression, and a `let` block, move the
destructuring into the `let` block. This does **not** check that the `let` block
is viable to be moved to and should only be used with a `let` block obtained
from `getValidLetBlock`.
-}
moveCasePatternToLetBlock : ( Expression, Range ) -> ( Node Pattern, Node Expression ) -> ( LetBlock, Range ) -> Fix
moveCasePatternToLetBlock ( caseExpr, caseRange ) ( replacePattern, replaceExpression ) ( { declarations, expression }, letRange ) =
    let
        go : Node Expression -> Expression
        go e =
            if Node.range e == caseRange then
                Node.value replaceExpression

            else
                mapSubexpressions (go >> Node emptyRange) <| Node.value e

        goDeclarations : Node LetDeclaration -> LetDeclaration
        goDeclarations n =
            case Node.value n of
                LetFunction ({ declaration } as d) ->
                    LetFunction
                        { d
                            | declaration =
                                Node.map (\r -> { r | expression = Node emptyRange <| go r.expression }) declaration
                        }

                LetDestructuring p e ->
                    LetDestructuring p <| Node emptyRange <| go e
    in
    go expression
        |> letExpr
            (List.map goDeclarations declarations
                ++ [ letDestructuring
                        (addParensToNamedPattern <| Node.value replacePattern)
                        caseExpr
                   ]
            )
        |> prettyExpressionReplacing letRange
        |> Fix.replaceRangeBy letRange


{-| Given a case expression and a single case pattern and expression, convert
the case into a `let` block destructured in.
-}
fixInNewLet : ( Expression, Range ) -> ( Node Pattern, Node Expression ) -> List Fix
fixInNewLet ( expressionInCaseOf, caseRange ) ( singleCasePattern, singleCaseExpression ) =
    [ Fix.replaceRangeBy caseRange
        (letExpr
            [ letDestructuring
                (addParensToNamedPattern <| Node.value singleCasePattern)
                expressionInCaseOf
            ]
            (Node.value singleCaseExpression)
            |> prettyExpressionReplacing caseRange
        )
    ]


{-| Given context, get the binding information of an expression if it consists
solely of a name and its binding location can be destructured at.
-}
getDestructurableBinding : LocalContext -> Expression -> Maybe ( String, Binding )
getDestructurableBinding { bindings } expr =
    case expr of
        FunctionOrValue [] name ->
            Dict.get name bindings
                |> MaybeX.filter .canDestructureAt
                |> Maybe.map (Tuple.pair name)

        _ ->
            Nothing


{-| Given context, the expression in a `case ... of`, and the single case
pattern and expression, get the closest `let` block to destructure in, if one
exists. This requires all names in expression to be in scope in the `let` and
checks for name clashes in the pattern to be moved.
-}
getValidLetBlock : LocalContext -> Expression -> ( Node Pattern, Node Expression ) -> Maybe ( LetBlock, Range )
getValidLetBlock { newBindingsSinceLastLet, closestLetBlock } caseExpr ( singleCasePattern, singleCaseExpr ) =
    if
        allBindingsUsedInExpression caseExpr
            |> Set.intersect newBindingsSinceLastLet
            |> Set.isEmpty
    then
        -- No bindings in expression weren't in scope at time of last `let`
        closestLetBlock
            |> Maybe.andThen
                (\{ expression, letBlock } ->
                    if
                        allBindingsInPattern (Node.value singleCaseExpr) singleCasePattern
                            |> List.any (\( n, _ ) -> nameUsedOutsideExpr n (Node.value singleCaseExpr) <| Node.value expression)
                    then
                        -- Cannot move due to name clash
                        Nothing

                    else
                        Just ( letBlock, Node.range expression )
                )

    else
        -- Some bindings in expression weren't in scope at time of last `let`
        Nothing


{-| Replace the entirety of a single-pattern case with the part after the
pattern, e.g.

    f x =
        case x of
            _ ->
                2

would become

    f x =
        2

-}
replaceCaseBlockWithExpression : SinglePatternCaseInfo -> Fix
replaceCaseBlockWithExpression { context, singleCaseExpression, caseRange } =
    Node.range singleCaseExpression
        |> context.extractSourceCode
        |> Fix.replaceRangeBy caseRange
