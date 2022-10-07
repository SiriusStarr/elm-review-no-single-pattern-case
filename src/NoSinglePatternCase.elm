module NoSinglePatternCase exposing
    ( rule
    , Config, fixInArgument, fixInLet
    , reportAllCustomTypes, replaceUnusedBindings, ifAsPatternRequired, ifCannotDestructureAtArgument, ifNoLetExists
    , fail, createNewLet, useAsPattern, fixInArgumentInstead, andIfAsPatternRequired, andIfCannotDestructureAtArgument, fixInLetInstead, andIfNoLetExists
    , FixInArgument, FixInLet, UseArgInstead, UseLetInstead, CreateNewLet, Fail, UseAsPattern, UseAsPatternOrFailOr, CreateNewLetOr, UseArgOrCreateNewLetOrFail, UseLetOr, UseLetOrFail, UseAsPatternOrLetsOrFail, Either
    )

{-|


## Notes

It is recommended that you also include
[`NoUnused.Patterns`](https://package.elm-lang.org/packages/jfmengels/elm-review-unused/latest/NoUnused-Patterns/)
in your config, as the fixes for this rule can sometimes generate nested `as`
patterns in very complex cases (e.g. when nested single-pattern cases are
simplified). These cases cannot be resolved automatically, as it is unclear
which name should be preferred.


## Review Rule

@docs rule


# Config

@docs Config, fixInArgument, fixInLet


## Customizing Config Behavior

@docs reportAllCustomTypes, replaceUnusedBindings, ifAsPatternRequired, ifCannotDestructureAtArgument, ifNoLetExists


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
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), LetBlock, LetDeclaration(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Range)
import Maybe.Extra as MaybeX
import Result.Extra as ResultX
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)
import Util
    exposing
        ( Binding
        , Destructuring
        , bindingsInPattern
        , either3
        , nameClash
        , nameUsedOutsideExprs
        , namesUsedInExpression
        , reduceDestructuring
        , reindent
        , subexpressions
        )


{-| Reports single-pattern case expressions, which may be written more concisely
or removed entirely.

    config =
        [ NoSinglePatternCase.rule NoSinglePatternCase.fixInArgument ]

See [`Config`](#Config) for configuration details.


## Fails

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

Single patterns with constructors that do not match their type name, e.g. `type
Msg = ButtonClicked`, are allowed by default, unless they are imported from
dependencies (as those types are not expected to be iterated on). This behavior
can be changed with [`reportAllCustomTypes`](#reportAllCustomTypes).

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
elm-review --template SiriusStarr/elm-review-no-single-pattern-case/example/fix-in-argument --rules NoSinglePatternCase
```

-}
rule : Config fixBy -> Rule
rule config =
    Rule.newProjectRuleSchema "NoSinglePatternCase" initialContext
        |> Rule.withModuleVisitor (moduleVisitor config)
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withContextFromImportedModules
        |> Rule.fromProjectRuleSchema


{-| Module context for the rule.
-}
type alias ModuleContext =
    { extractSourceCode : Range -> String
    , lookupTable : ModuleNameLookupTable
    , nonWrappedTypes : Dict ModuleName (Set String)
    }


{-| Project context for the rule.
-}
type alias ProjectContext =
    { nonWrappedTypes : Dict ModuleName (Set String)
    }


{-| The initial project context.
-}
initialContext : ProjectContext
initialContext =
    { nonWrappedTypes = Dict.empty }


{-| Visit each module, first getting types from all declarations and then
checking all for `case`s.
-}
moduleVisitor : Config fixBy -> Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor config schema =
    schema
        |> Rule.withDeclarationListVisitor (\ds c -> ( [], declarationListVisitor config ds c ))
        |> Rule.withDeclarationEnterVisitor (checkDeclaration config)


{-| Create a `ProjectContext` from a `ModuleContext`.
-}
fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleName moduleContext ->
            { nonWrappedTypes =
                moduleContext.nonWrappedTypes
                    |> Dict.get []
                    |> Maybe.withDefault Set.empty
                    |> Dict.singleton moduleName
            }
        )
        |> Rule.withModuleName


{-| Create a `ModuleContext` from a `ProjectContext`.
-}
fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\extractSourceCode lookupTable projectContext ->
            { extractSourceCode = extractSourceCode
            , lookupTable = lookupTable
            , nonWrappedTypes = projectContext.nonWrappedTypes
            }
        )
        |> Rule.withSourceCodeExtractor
        |> Rule.withModuleNameLookupTable


{-| Combine `ProjectContext`s.
-}
foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext prevContext =
    { nonWrappedTypes =
        Dict.union newContext.nonWrappedTypes prevContext.nonWrappedTypes
    }


{-| Visit declarations, storing any types that should not be reduced.
-}
declarationListVisitor : Config fixBy -> List (Node Declaration) -> ModuleContext -> ModuleContext
declarationListVisitor (Config { reportAllTypes }) declarations context =
    let
        getNonWrappedType : Node Declaration -> Maybe String
        getNonWrappedType node =
            case Node.value node of
                CustomTypeDeclaration { name, constructors } ->
                    case constructors of
                        [ c ] ->
                            let
                                n : String
                                n =
                                    Node.value (Node.value c).name
                            in
                            if n /= Node.value name then
                                Just n

                            else
                                Nothing

                        _ ->
                            -- More than one constructor can't be a wrapped type
                            Nothing

                _ ->
                    Nothing
    in
    if reportAllTypes then
        -- Don't store any if we're reporting everything
        context

    else
        -- Find non-wrapped custom types that were defined in the module, and store them in the context.
        { context
            | nonWrappedTypes =
                List.filterMap getNonWrappedType declarations
                    |> (\ts ->
                            if List.isEmpty ts then
                                context.nonWrappedTypes

                            else
                                Dict.insert [] (Set.fromList ts) context.nonWrappedTypes
                       )
        }


{-| Configure the rule, determining how automatic fixes are generated.

The default `Config`s [`fixInArgument`](#fixInArgument) and
[`fixInLet`](#fixInLet) should be used as reasonable defaults, with more
customization detailed in those sections.

The behavior of the rule with constructors that don't match their type name can
be configured via [`reportAllCustomTypes`](#reportAllCustomTypes). By default,
only constructors with an identical name to their type are reported.

The behavior of the rule in the context of useless single pattern cases can also
be configured via [`replaceUnusedBindings`](#replaceUnusedBindings). A single
pattern case is considered to be useless if its pattern does not bind any name
that is actually used in the expression.

-}
type Config fixBy
    = Config
        { fixBy : FixBy
        , replaceUseless : Bool
        , reportAllTypes : Bool
        }


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
        , replaceUseless = False
        , reportAllTypes = False
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
        { fixBy = DestructureInLet { ifNoLetBlock = createNewLet }
        , replaceUseless = False
        , reportAllTypes = False
        }


{-| By default, only constructors whose names are identical to their type are
reported, along with types imported from dependencies (since those aren't
expected to be iterated on). This setting changes behavior back to that of
version `2.0.2` and earlier, where absolutely all single pattern cases are
flagged by the rule, regardless of the types.

For instance, _both_ of the cases in the following are flagged when this setting
is used.

    -- import the constructor `OutsideConstructor` from some other package


    import SomeOutsidePackage exposing (OutsideType(..))

    type Date
        = -- Constructor has same name as type
          Date Int

    type Msg
        = -- Constructor has different name than type
          ThingieClicked

    update1 : Date -> Int -> Int
    update1 date i =
        case date of
            -- THIS CASE IS ALWAYS FLAGGED
            Date j ->
                i + j

    update2 : Msg -> Int -> Int
    update2 msg i =
        case msg of
            -- THIS CASE IS NOT FLAGGED BY DEFAULT
            ThingieClicked ->
                i + 1

    update3 : OutsideType -> Int -> Int
    update3 oType i =
        case oType of
            -- THIS CASE IS ALWAYS FLAGGED
            OutsideConstructor j ->
                i + j

-}
reportAllCustomTypes : Config fixBy -> Config fixBy
reportAllCustomTypes (Config r) =
    Config { r | reportAllTypes = True }


{-| A single pattern case is considered to be useless if its pattern does not
bind any name that is actually used in the expression, e.g.

    case x of
        _ ->
            True

    case x of
        () ->
            True

    case x of
        A ({ field1, field2 } as record) ->
            List.map foo bar
                |> List.sum
                |> baz

The rule will always provide fixes for such cases but by default will not
replace the binding used in the `case...of` expression. This option configures
the rule to replace such bindings where possible. Fox example:

    f unusedArg =
        case unusedArg of
            _ ->
                True

will be fixed to

    f _ =
        True

This provides a more clear indication that the binding is unused. The binding
will of course **not** be replaced if it used anywhere but in `case...of`.

-}
replaceUnusedBindings : Config fixBy -> Config fixBy
replaceUnusedBindings (Config r) =
    Config { r | replaceUseless = True }


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
                    List.concatMap (bindingsInPattern expression) arguments
                        |> Dict.fromList
                , closestLetBlock = Nothing
                , newBindingsSinceLastLet = Set.empty
                , moduleContext = context
                }
                expression

        _ ->
            []
    , context
    )


{-| The local context in which a single-pattern case exists.

  - `bindings` -- All bindings in local scope.
  - `closestLetBlock` -- The closest `let` block to the single-pattern case,
    i.e. the one most closely "above" in the AST.
  - `newBindingsSinceLastLet` -- All new bindings added since the last `let`
    block (that could cause a name clash if a new name were added at the `let`).
  - `moduleContext` -- The module context.

-}
type alias LocalContext =
    { bindings : Dict String Binding
    , closestLetBlock :
        Maybe
            { expression : Node Expression
            , letBlock : LetBlock
            }
    , newBindingsSinceLastLet : Set String
    , moduleContext : ModuleContext
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
            (case caseBlock.cases of
                [ ( p, e ) ] ->
                    reduceDestructuring
                        { bindings = context.bindings
                        , nonWrappedTypes = context.moduleContext.nonWrappedTypes
                        , lookupTable = context.moduleContext.lookupTable
                        , outputExpression = e
                        }
                        p
                        caseBlock.expression
                        |> (\({ removableBindings, usefulPatterns, removedExpressions } as destructuring) ->
                                if List.isEmpty removableBindings && List.isEmpty usefulPatterns && List.isEmpty removedExpressions then
                                    -- No error if it is fully ignorable
                                    []

                                else
                                    -- Report error and rewrite case
                                    [ singlePatternCaseError config
                                        { context = context
                                        , outputExpression = e
                                        , caseRange = Node.range expressionNode
                                        , destructuring = destructuring
                                        , errorRange = Node.range p
                                        }
                                    ]
                           )
                        -- Add pattern match bindings and descend into output expression
                        |> (\err -> err ++ go Nothing (bindingsInPattern e p) e)

                multipleCases ->
                    multipleCases
                        |> List.concatMap
                            (\( p, e ) ->
                                -- Add pattern match bindings
                                go Nothing (bindingsInPattern e p) e
                            )
            )
                ++ -- Check expression in case...of as well
                   go Nothing [] caseBlock.expression

        LetExpression lB ->
            let
                checkDecl : Node LetDeclaration -> ( List ( String, Binding ), List ( String, Binding ) -> List (Error {}) )
                checkDecl d =
                    case Node.value d of
                        LetFunction fun ->
                            let
                                { name, expression, arguments } =
                                    Node.value fun.declaration
                            in
                            ( [ ( Node.value name
                                , { patternNodeRange = Node.range name
                                  , canDestructureAt = fun.signature == Nothing
                                  , scope = expressionNode
                                  }
                                )
                              ]
                            , List.concatMap (bindingsInPattern expression) arguments
                                |> (\args bs ->
                                        go (Just lB) (args ++ bs) expression
                                   )
                            )

                        LetDestructuring pattern expr ->
                            ( bindingsInPattern expressionNode pattern
                            , \bs -> go (Just lB) bs expr
                            )

                ( letBindings, subexprs ) =
                    -- Gather all new bindings and subexpressions to check in one pass
                    List.foldl
                        (\d ( bAcc, eAcc ) ->
                            checkDecl d
                                |> (\( bs, e ) -> ( bs ++ bAcc, e :: eAcc ))
                        )
                        ( [], [] )
                        lB.declarations
            in
            go (Just lB) letBindings lB.expression
                ++ List.concatMap (\f -> f letBindings) subexprs

        LambdaExpression { args, expression } ->
            -- Add arg bindings
            List.concatMap (bindingsInPattern expression) args
                |> (\bs ->
                        go Nothing bs expression
                   )

        _ ->
            subexpressions expressionNode
                |> List.concatMap (go Nothing [])


{-| Information about a detected single-pattern case.

  - `destructuring` -- The fully-reduced destructuring occurring in the case.
  - `context` -- The local context the expression exists in (bindings, closest
    `let` block. etc.)
  - `outputExpression` -- The expression output by the single pattern.
  - `caseRange` -- The `Range` of the entire case expression.
  - `errorRange` -- The `Range` to report the error at.

-}
type alias SinglePatternCase =
    { destructuring : Destructuring
    , context : LocalContext
    , outputExpression : Node Expression
    , caseRange : Range
    , errorRange : Range
    }


{-| A binding that can be directly destructured at.

  - `requiredAsName` -- The `as` pattern required when replacing the binding, if
    any.
  - `binding` -- The `Binding` at which destructuring can occur.
  - `fallbackExpression` -- The original expression from which this binding was
    generated, in case we want to destructure that instead of replacing the
    binding.

-}
type alias DestructurableBinding =
    { requiredAsName : Maybe String
    , binding : Binding
    , fallbackExpression : Node Expression
    }


{-| An error for when a case expression only contains one case pattern. See [`Config`](NoSinglePatternCase#Config) for how fixes will be generated.
-}
singlePatternCaseError : Config fixBy -> SinglePatternCase -> Error {}
singlePatternCaseError config info =
    makeFix config info
        |> Rule.errorWithFix
            { message = "Single pattern case block."
            , details = [ "Single pattern case blocks typically are either unnecessary or overly verbose.  There's usually a more concise way to destructure, e.g. in a function argument, so consider refactoring." ]
            }
            info.errorRange


{-| Given config and info about a single pattern case, try to create a fix per
the config or fail.
-}
makeFix : Config fixBy -> SinglePatternCase -> List Fix
makeFix (Config { fixBy, replaceUseless }) ({ destructuring } as info) =
    let
        destructureInLet :
            { ifNoLetBlock : Either (Either CreateNewLet noLetFix) Fail
            , noLetFix : noLetFix -> List ( Node Pattern, Node Expression ) -> Maybe (List Fix)
            }
            -> List ( Node Pattern, Node Expression )
            -> Maybe (List Fix)
        destructureInLet { ifNoLetBlock, noLetFix } ps =
            let
                { fixedInLet, noSuitableLetExists } =
                    destructureInExistingLet info ps

                noLetFixes : Maybe (List Fix)
                noLetFixes =
                    noSuitableLetExists
                        |> fixRemaining
                            (either3 ifNoLetBlock
                                useNewLet
                                noLetFix
                                orFail
                            )
            in
            combineFixes
                [ fixedInLet
                , noLetFixes
                ]

        useNewLet : CreateNewLet -> List ( Node Pattern, Node Expression ) -> Maybe (List Fix)
        useNewLet CreateNewLet =
            Just << moveDestructuringsToNewLetBlock info

        fallbackToArg : UseArgInstead -> List ( Node Pattern, Node Expression ) -> Maybe (List Fix)
        fallbackToArg (UseArgInstead { ifAsPatternNeeded, ifCannotDestructure }) =
            destructureInArg
                { asPatternAlternative = useNewLet
                , ifAsPatternNeeded = ifAsPatternNeeded
                , ifCannotDestructure = ifCannotDestructure
                , cannotDestructureFix = useNewLet
                }

        orFail : Fail -> List ( Node Pattern, Node Expression ) -> Maybe (List Fix)
        orFail Fail _ =
            Nothing

        fallbackToLet : (noLetFix -> List ( Node Pattern, Node Expression ) -> Maybe (List Fix)) -> UseLetInstead (Either (Either CreateNewLet noLetFix) Fail) -> List ( Node Pattern, Node Expression ) -> Maybe (List Fix)
        fallbackToLet noLetFix (UseLetInstead { ifNoLetBlock }) =
            destructureInLet
                { ifNoLetBlock = ifNoLetBlock
                , noLetFix = noLetFix
                }

        orUseAsPattern : UseAsPattern -> List ( Node Pattern, Node Expression ) -> Maybe (List Fix)
        orUseAsPattern UseAsPattern ps =
            let
                { fixedInArg, useAsFallback, notDestructurable } =
                    -- Force `as` pattern usage
                    destructureAtBindings info True ps
            in
            case ( fixedInArg, useAsFallback, notDestructurable ) of
                ( fixes, [], [] ) ->
                    -- If this handled everything, we're okay
                    fixes

                _ ->
                    -- Some patterns were not destructurable, so fail
                    Nothing

        destructureInArg :
            { asPatternAlternative : asPatternAlternative -> List ( Node Pattern, Node Expression ) -> Maybe (List Fix)
            , ifAsPatternNeeded : UseAsPatternOrFailOr asPatternAlternative
            , ifCannotDestructure : Either (Either destructureFallback Fail) Fail
            , cannotDestructureFix : destructureFallback -> List ( Node Pattern, Node Expression ) -> Maybe (List Fix)
            }
            -> List ( Node Pattern, Node Expression )
            -> Maybe (List Fix)
        destructureInArg { asPatternAlternative, ifAsPatternNeeded, ifCannotDestructure, cannotDestructureFix } ps =
            let
                { fixedInArg, useAsFallback, notDestructurable } =
                    let
                        useAs : Bool
                        useAs =
                            either3 ifAsPatternNeeded
                                (always False)
                                (always True)
                                (always False)
                    in
                    destructureAtBindings info useAs ps

                asFallbackFixes : Maybe (List Fix)
                asFallbackFixes =
                    List.map (Tuple.mapSecond .fallbackExpression) useAsFallback
                        |> fixRemaining
                            (either3 ifAsPatternNeeded
                                asPatternAlternative
                                -- This should never happen
                                (\_ _ -> Nothing)
                                orFail
                            )

                notDestructurableFixes : Maybe (List Fix)
                notDestructurableFixes =
                    notDestructurable
                        |> fixRemaining (either3 ifCannotDestructure cannotDestructureFix orFail orFail)
            in
            combineFixes
                [ fixedInArg
                , asFallbackFixes
                , notDestructurableFixes
                ]

        fixRemaining : (List ( Node Pattern, Node Expression ) -> Maybe (List Fix)) -> List ( Node Pattern, Node Expression ) -> Maybe (List Fix)
        fixRemaining fallback ps =
            if List.isEmpty ps then
                Just []

            else
                fallback ps

        combineFixes : List (Maybe (List Fix)) -> Maybe (List Fix)
        combineFixes =
            MaybeX.combine
                >> Maybe.map List.concat
    in
    (case fixBy of
        DestructureInLet { ifNoLetBlock } ->
            destructureInLet
                { noLetFix = fallbackToArg
                , ifNoLetBlock = ifNoLetBlock
                }
                destructuring.usefulPatterns

        DestructureInArgument { ifAsPatternNeeded, ifCannotDestructure } ->
            destructureInArg
                { asPatternAlternative = fallbackToLet orUseAsPattern
                , ifAsPatternNeeded = ifAsPatternNeeded
                , ifCannotDestructure = ifCannotDestructure
                , cannotDestructureFix = fallbackToLet orFail
                }
                destructuring.usefulPatterns
    )
        |> Maybe.map
            (\fs ->
                (if replaceUseless then
                    fixUselessBindings destructuring.removableBindings

                 else
                    []
                )
                    ++ fs
            )
        -- If fixes succeeded, replace the case block with the single expression (or any ignored patterns)
        |> MaybeX.unwrap [] ((::) (rewriteCaseExpression info))


{-| Given a `SinglePatternCase` and a list of patterns and expressions to
destructure in the closest existing `let` block, return a list of fixes moving
patterns to that `let` (or `Nothing` if that failed) as well as any remaining
patterns that could not be fixed in it.
-}
destructureInExistingLet :
    SinglePatternCase
    -> List ( Node Pattern, Node Expression )
    ->
        { fixedInLet : Maybe (List Fix)
        , noSuitableLetExists : List ( Node Pattern, Node Expression )
        }
destructureInExistingLet ({ context } as info) patterns =
    let
        { hasValidLet, lacksValidLet } =
            getValidLetBlock info patterns
    in
    { fixedInLet =
        MaybeX.unwrap []
            (\( lb, ps ) ->
                moveDestructuringsToExistingLetBlock context.moduleContext lb ps
            )
            hasValidLet
            |> Just
    , noSuitableLetExists = lacksValidLet
    }


{-| Given a `SinglePatternCase`, whether or not to use `as` patterns if
necessary, and a list of patterns and expressions to destructure at the bindings
of (if possible), return a list of fixes destructuring patterns at bindings, as
well as any patterns that needed `as` patterns (if told not to use them), and
any remaining patterns that could not be destructured in this fashion.
-}
destructureAtBindings :
    SinglePatternCase
    -> Bool
    -> List ( Node Pattern, Node Expression )
    ->
        { fixedInArg : Maybe (List Fix)
        , useAsFallback : List ( Node Pattern, DestructurableBinding )
        , notDestructurable : List ( Node Pattern, Node Expression )
        }
destructureAtBindings { context, destructuring, outputExpression } destructureUsingAsPattern patterns =
    let
        { canDestructure, cannotDestructure } =
            let
                ( can, cannot ) =
                    patterns
                        |> List.map
                            (\( p, e ) ->
                                getValidDestructurableBinding context
                                    { pattern = p
                                    , ignoreNameUsesIn = destructuring.removedExpressions
                                    , destructuredExpression = e
                                    , outputExpression = outputExpression
                                    }
                                    |> Result.fromMaybe ( p, e )
                                    |> Result.map (Tuple.pair p)
                            )
                        |> ResultX.partition
            in
            checkForCollidingBindings can
                |> (\( c, collided ) ->
                        { canDestructure = c
                        , cannotDestructure = cannot ++ List.concat collided
                        }
                   )

        ( moveToArg, useAsFallback ) =
            if destructureUsingAsPattern then
                ( canDestructure, [] )

            else
                List.partition ((==) Nothing << .requiredAsName << Tuple.second) canDestructure
    in
    { fixedInArg = Just <| List.map (movePatternToBinding context) moveToArg
    , useAsFallback = useAsFallback
    , notDestructurable = cannotDestructure
    }


{-| Given a list of patterns and their locations to destructure at, partition
them into those whose binding sites do not overlap and those that would conflict
with each other.
-}
checkForCollidingBindings : List ( Node Pattern, DestructurableBinding ) -> ( List ( Node Pattern, DestructurableBinding ), List (List ( Node Pattern, Node Expression )) )
checkForCollidingBindings =
    let
        overlaps : Range -> Range -> Bool
        overlaps r1 r2 =
            -- Check if the two binding ranges overlap at all
            if Range.compareLocations r1.end r2.start == LT then
                False

            else
                Range.compareLocations r2.end r1.start /= LT

        go : ( List ( Node Pattern, DestructurableBinding ), List (List ( Node Pattern, Node Expression )) ) -> List ( Node Pattern, DestructurableBinding ) -> ( List ( Node Pattern, DestructurableBinding ), List (List ( Node Pattern, Node Expression )) )
        go ( cleared, collided ) remaining =
            case remaining of
                [] ->
                    ( List.reverse cleared, List.reverse collided )

                (( _, currBinding ) as b) :: bs ->
                    case
                        List.partition (\( _, { binding } ) -> overlaps binding.patternNodeRange currBinding.binding.patternNodeRange) bs
                    of
                        ( [], uncollided ) ->
                            go ( b :: cleared, collided ) uncollided

                        ( collisions, uncollided ) ->
                            go ( cleared, List.map (Tuple.mapSecond .fallbackExpression) (b :: collisions) :: collided ) uncollided
    in
    go ( [], [] )


{-| Given local context, a pattern node, and a binding to destructure at,
generate a fix that destructures at the binding. This function does not check if
this is possible, and should only be called after
`getValidDestructurableBinding`.
-}
movePatternToBinding : LocalContext -> ( Node Pattern, DestructurableBinding ) -> Fix
movePatternToBinding { moduleContext } ( pat, { requiredAsName, binding } ) =
    Node.range pat
        |> moduleContext.extractSourceCode
        |> (\p ->
                case requiredAsName of
                    Just name ->
                        String.concat [ "(", p, ") as ", name ]

                    Nothing ->
                        p
           )
        |> (\p -> "(" ++ p ++ ")")
        |> Fix.replaceRangeBy binding.patternNodeRange


{-| Remove all bindings that are in the `case...of` expression that are not used
elsewhere else.
-}
fixUselessBindings : List { isUnit : Bool, binding : Binding } -> List Fix
fixUselessBindings =
    List.map
        (\{ isUnit, binding } ->
            (if isUnit then
                "()"

             else
                "_"
            )
                |> Fix.replaceRangeBy binding.patternNodeRange
        )


{-| Given context, a pattern, the expression it destructures, and a list of
expressions to ignore uses in (for `as` patterns), and the output expression of
case, return a `DestructurableBinding` the pattern can be moved to, if possible
(checking for name clashes and the like).
-}
getValidDestructurableBinding :
    LocalContext
    ->
        { pattern : Node Pattern
        , destructuredExpression : Node Expression
        , ignoreNameUsesIn : List (Node Expression)
        , outputExpression : Node Expression
        }
    -> Maybe DestructurableBinding
getValidDestructurableBinding { bindings } { pattern, destructuredExpression, ignoreNameUsesIn, outputExpression } =
    let
        getBinding : Node Expression -> Maybe ( String, Binding )
        getBinding expr =
            case Node.value expr of
                FunctionOrValue [] name ->
                    Dict.get name bindings
                        |> MaybeX.filter .canDestructureAt
                        |> Maybe.map (Tuple.pair name)

                ParenthesizedExpression e ->
                    getBinding e

                _ ->
                    Nothing
    in
    getBinding destructuredExpression
        |> Maybe.andThen
            (\( name, b ) ->
                if
                    nameClash
                        { insideExpr = outputExpression :: ignoreNameUsesIn
                        , scope = b.scope
                        }
                        pattern
                then
                    -- Cannot move the pattern if it would cause name clash
                    Nothing

                else
                    Just
                        { requiredAsName =
                            if
                                nameUsedOutsideExprs
                                    { inside = destructuredExpression :: ignoreNameUsesIn
                                    , scope = b.scope
                                    }
                                    name
                            then
                                Just name

                            else
                                Nothing
                        , binding = b
                        , fallbackExpression = destructuredExpression
                        }
            )


{-| Given context, a `LetBlock`, and a list of patterns and expressions to
destructure, destructure them at the end of the `LetBlock`.
-}
moveDestructuringsToExistingLetBlock : ModuleContext -> LetBlock -> List ( Node Pattern, Node Expression ) -> List Fix
moveDestructuringsToExistingLetBlock moduleContext { declarations } ps =
    if List.isEmpty ps then
        -- Don't bother fixing empty lists; this should be prevented up-stream,
        -- but better safe than sorry
        []

    else
        let
            oldDeclarationRange : Range
            oldDeclarationRange =
                List.map Node.range declarations
                    |> Range.combine
        in
        [ makeLetDecs moduleContext (oldDeclarationRange.start.column - 1) ps
            |> (\ds -> "\n\n" ++ ds)
            |> Fix.insertAt oldDeclarationRange.end
        ]


{-| Given a single pattern case and a list of patterns and expressions to
destructure, create a new `let` to destructure them in.
-}
moveDestructuringsToNewLetBlock : SinglePatternCase -> List ( Node Pattern, Node Expression ) -> List Fix
moveDestructuringsToNewLetBlock { caseRange, context } ps =
    if List.isEmpty ps then
        -- Don't bother fixing empty lists; this should be prevented up-stream,
        -- but better safe than sorry
        []

    else
        -- Leading spaces here are so that we leave an empty, indented line at for
        -- when we replace the expression later
        [ [ " let"
          , makeLetDecs context.moduleContext 5 ps
          , " in"
          , " "
          ]
            |> String.join "\n"
            |> reindent (caseRange.start.column - 1)
            |> Fix.insertAt caseRange.start
        ]


{-| Given context, an indent amount, and a list of patterns and expressions to
destructure, create `let` declarations that destructure them.
-}
makeLetDecs : ModuleContext -> Int -> List ( Node Pattern, Node Expression ) -> String
makeLetDecs { extractSourceCode } letIndentAmt =
    List.map
        (\( pat, expr ) ->
            let
                e : String
                e =
                    Node.range expr
                        |> extractSourceCode
                        |> reindent (letIndentAmt + 9 - (Node.range expr).start.column)

                indent : String
                indent =
                    String.repeat letIndentAmt " "
            in
            String.concat
                [ indent
                , "("
                , extractSourceCode <| Node.range pat
                , ") =\n"
                , indent
                , "    "
                , e
                ]
        )
        >> String.join "\n\n"


{-| Given a `SinglePaternCase` and a list of patterns/expressions to
destructure, get the closest `let` block to destructure in and return those that
can be destructured there and those that can't. This requires all names in
expression to be in scope in the `let` and checks for name clashes in the
pattern to be moved.
-}
getValidLetBlock : SinglePatternCase -> List ( Node Pattern, Node Expression ) -> { hasValidLet : Maybe ( LetBlock, List ( Node Pattern, Node Expression ) ), lacksValidLet : List ( Node Pattern, Node Expression ) }
getValidLetBlock { context, destructuring, outputExpression } ps =
    case context.closestLetBlock of
        Nothing ->
            { hasValidLet = Nothing
            , lacksValidLet = ps
            }

        Just { expression, letBlock } ->
            let
                ignorableExpressions : List (Node Expression)
                ignorableExpressions =
                    outputExpression :: destructuring.removedExpressions ++ List.map Tuple.second ps
            in
            ps
                |> List.foldr
                    (\( p, e ) ( hasLetAcc, noLetAcc ) ->
                        if
                            namesUsedInExpression e
                                |> Set.intersect context.newBindingsSinceLastLet
                                |> Set.isEmpty
                        then
                            -- No bindings in expression weren't in scope at time of last `let`
                            if
                                nameClash
                                    { insideExpr = ignorableExpressions
                                    , scope = expression
                                    }
                                    p
                            then
                                -- Cannot move due to name clash
                                ( hasLetAcc, ( p, e ) :: noLetAcc )

                            else
                                ( ( p, e ) :: hasLetAcc, noLetAcc )

                        else
                            -- Some bindings in expression weren't in scope at time of last `let`
                            ( hasLetAcc, ( p, e ) :: noLetAcc )
                    )
                    ( [], [] )
                |> (\( has, lacks ) ->
                        { hasValidLet =
                            if List.isEmpty has then
                                Nothing

                            else
                                Just <| Tuple.pair letBlock has
                        , lacksValidLet = lacks
                        }
                   )


{-| Replace the entirety of a single-pattern case with a rewritten version that
includes only ignored patterns (if any), e.g.

    f x =
        case x of
            _ ->
                2

would become

    f x =
        2

-}
rewriteCaseExpression : SinglePatternCase -> Fix
rewriteCaseExpression { context, outputExpression, destructuring, caseRange } =
    let
        output : String
        output =
            Node.range outputExpression
                |> context.moduleContext.extractSourceCode
    in
    case destructuring.ignoredPatterns of
        [] ->
            Fix.replaceRangeBy caseRange output

        rem ->
            let
                ps : String
                ps =
                    List.map (context.moduleContext.extractSourceCode << Node.range << Tuple.first) rem
                        |> String.join ", "

                es : String
                es =
                    List.map (context.moduleContext.extractSourceCode << Node.range << Tuple.second) rem
                        |> String.join ", "
            in
            String.concat
                [ "case ("
                , es
                , ") of\n    ("
                , ps
                , ") ->\n        "
                , output
                ]
                |> reindent caseRange.start.column
                |> Fix.replaceRangeBy caseRange
