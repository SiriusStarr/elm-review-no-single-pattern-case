module NoSinglePatternCase exposing
    ( rule
    , Config
    , CreateSeparateLet, DontCreateSeparateLet
    , alwaysFixInArgument, alwaysFixInLet
    , fixByDestructuringInExistingLets, fixByDestructuringTheArgument
    , destructureInExistingLets, destructureUsingAs, destructureTheArgument, destructureUsingAsInstead, destructureInExistingLetsInstead
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
@docs destructureInExistingLets, destructureUsingAs, destructureTheArgument, destructureUsingAsInstead, destructureInExistingLetsInstead


### separate let is only option left

@docs noFix, createSeparateLet


### on name clash

@docs createSeparateLetOnNameClash, noFixOnNameClash

-}

import Elm.CodeGen exposing (asPattern, letDestructuring, letExpr)
import Elm.Pretty exposing (prettyExpression)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range, emptyRange)
import Pretty exposing (pretty)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)
import SyntaxHelp exposing (VarPatternKind(..), allVarsInPattern, expressionsInExpression, parensAroundNamedPattern, prettyPrintPattern, updateExpressionsInExpression, usesIn)


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
rule : Config separateLetAllowed -> Rule
rule destructuringScope =
    Rule.newModuleRuleSchema "NoSinglePatternCase" ()
        |> Rule.withSimpleDeclarationVisitor
            (Node.value >> checkDeclaration destructuringScope)
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

All options:

  - [`fixByDestructuringInExistingLets`](NoSinglePatternCase#fixByDestructuringInExistingLets)
      - | If no lets exist
          - [`destructureTheArgument`](NoSinglePatternCase#destructureTheArgument)
              - | If the argument pattern is a record field pattern or `as` destructured, use [_"only separate let fix left"_](#only-separate-let-fix-left)
              - | If between `case` and `of` is not just an argument, use [_"only separate let fix left"_](#only-separate-let-fix-left)
              - | If the argument is also used elsewhere
                  - [`destructureUsingAs`](NoSinglePatternCase#destructureUsingAs)
                  - [_"only separate let fix left"_](#only-separate-let-fix-left)
          - [_"only separate let fix left"_](#only-separate-let-fix-left)
  - [`fixByDestructuringTheArgument`](NoSinglePatternCase#fixByDestructuringTheArgument)
      - | If the argument is also used elsewhere
          - [`destructureUsingAsInstead`](NoSinglePatternCase#destructureUsingAsInstead)
          - [`destructureInExistingLetsInstead`](NoSinglePatternCase#destructureInExistingLetsInstead)
              - | If no lets exist
                  - [`destructureUsingAs`](NoSinglePatternCase#destructureUsingAs)
                  - [_"only separate let fix left"_](#only-separate-let-fix-left)
      - | If between `case` and `of` is not just an argument or if the argument pattern is a record field pattern or `as` destructured
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
type Config separateLetAllowed
    = Config
        (FixByDestructuringIn separateLetAllowed)
        (OnlyCreatingSeparateLetLeft
            separateLetAllowed
        )


{-| Resolve the cases

  - name clash.
  - `OnlyCreatingSeparateLetLeftFix`.

-}
type OnlyCreatingSeparateLetLeft separateLetAllowed
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


type OrOnlyCreatingSeparateLetLeft option separateLetAllowed
    = Fix option
    | OnlyCreatingSeparateLetLeftFix


{-| Specify how to automatically fix single-pattern cases in specific conditions.
-}
type FixByDestructuringIn separateLetAllowed
    = FixByDestructuringInExistingLets
        { noExistingLets :
            OrOnlyCreatingSeparateLetLeft
                (DestructureTheArgument separateLetAllowed)
                separateLetAllowed
        }
    | FixByDestructuringTheArgument
        { argumentAlsoUsedElsewhere :
            ArgumentAlsoUsedElsewhere separateLetAllowed
        , notDestructable :
            --| If between `case` and `of` isn't just an argument or
            --| record field pattern or `as` destructured
            OrOnlyCreatingSeparateLetLeft
                -- If no lets exist, use _separateLetAllowed_
                DestructureInExistingLets
                separateLetAllowed
        }


type DestructureTheArgument separateLetAllowed
    = DestructureTheArgument
        { argumentAlsoUsedElsewhere :
            OrOnlyCreatingSeparateLetLeft
                DestructureUsingAs
                separateLetAllowed

        -- If between `case` and `of` is not just an argument, use _separateLetAllowed_
        -- if the argument is a record field pattern or `as` destructured, use _separateLetAllowed_
        }


type DestructureUsingAs
    = DestructureUsingAs


type DestructureInExistingLets
    = DestructureInExistingLets


type ArgumentAlsoUsedElsewhere separateLetAllowed
    = DestructureUsingAsInstead
    | DestructureInExistingLetsInstead
        { noExistingLets :
            OrOnlyCreatingSeparateLetLeft
                DestructureUsingAs
                separateLetAllowed
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
    FixByDestructuringIn CreateSeparateLet
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
    FixByDestructuringIn DontCreateSeparateLet
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

All options:

  - | If no lets exist
      - [`destructureTheArgument`](NoSinglePatternCase#destructureTheArgument)
          - | If the var pattern is a record field pattern or `as` destructured, use your fix for the situation "only separate let fix left"
          - | If between `case` and `of` is not just an argument, use your fix for the situation "only separate let fix left"
          - | If the argument is also used elsewhere
              - [`destructureUsingAs`](NoSinglePatternCase#destructureUsingAs)
              - your fix for the situation "only separate let fix left"
      - your fix for the situation "only separate let fix left"

examples

    fixByDestructuringInExistingLets
        { noExistingLets =
            destructureTheArgument
                { argumentAlsoUsedElsewhere =
                    destructureUsingAs
                }
        }

    onlyFixInExistingLets =
        fixByDestructuringInExistingLets
            { noExistingLets = noFix }
            |> noFixOnNameClash

-}
fixByDestructuringInExistingLets :
    { noExistingLets :
        OrOnlyCreatingSeparateLetLeft
            (DestructureTheArgument separateLetAllowed)
            separateLetAllowed
    }
    -> FixByDestructuringIn separateLetAllowed
fixByDestructuringInExistingLets noExistingLets =
    FixByDestructuringInExistingLets noExistingLets


{-| Destructure the argument.

    before o =
        case o of
            Opaque i ->
                i

    after (Opaque i) =
        i

All options:

  - | If the argument is also used elsewhere
      - [`destructureUsingAsInstead`](NoSinglePatternCase#destructureUsingAsInstead)
      - [`destructureInExistingLetsInstead`](NoSinglePatternCase#destructureInExistingLetsInstead)
          - | If no lets exist
              - [`destructureUsingAs`](NoSinglePatternCase#destructureUsingAs)
              - your fix for the situation "only separate let fix left"
  - | If the expression between `case` and `of` is not an argument or the argument pattern is a record field pattern or `as` destructured
      - [`destructureInExistingLets`](NoSinglePatternCase#destructureInExistingLets)
          - | If no lets exist, use your fix for the situation "only separate let fix left"
      - your fix for the situation "only separate let fix left"

For example

    replaceArgumentOrElseDestructureInLet =
        fixByDestructuringTheArgument
            { argumentAlsoUsedElsewhere =
                destructureInExistingLetsInstead
                    { noExistingLets =
                        createSeparateLet
                    }
            , notDestructable =
                destructureInExistingLets
            }
            |> createSeparateLetOnNameClash

-}
fixByDestructuringTheArgument :
    { argumentAlsoUsedElsewhere :
        ArgumentAlsoUsedElsewhere separateLetAllowed
    , notDestructable :
        OrOnlyCreatingSeparateLetLeft
            DestructureInExistingLets
            separateLetAllowed
    }
    -> FixByDestructuringIn separateLetAllowed
fixByDestructuringTheArgument exceptions =
    FixByDestructuringTheArgument exceptions


{-| destructure after existing let declarations.
| If no lets exist, uses your fix for the situation "only separate let fix left".

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

For example

    fixByDestructuringTheArgument
        { argumentAlsoUsedElsewhere =
            destructureUsingAsInstead
        , notDestructable =
            destructureInExistingLets
        }

-}
destructureInExistingLets :
    OrOnlyCreatingSeparateLetLeft
        DestructureInExistingLets
        separateLetAllowed
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
                    destructureUsingAs
                }
        }

-}
destructureUsingAs :
    OrOnlyCreatingSeparateLetLeft
        DestructureUsingAs
        separateLetAllowed
destructureUsingAs =
    Fix DestructureUsingAs


{-| Replace the argument with the pattern of the single case. All options:

  - | If the argument is a record field pattern or `as` destructured, uses your fix for the situation "only separate let fix left".
  - | If between `case` and `of` is not just an argument, uses your fix for the situation "only separate let fix left"
  - | If the argument is also used elsewhere
      - [`destructureUsingAs`](NoSinglePatternCase#destructureUsingAs)
      - your fix for the situation "only separate let fix left"

For example

    destructureTheArgument
        { argumentAlsoUsedElsewhere = destructureUsingAs }

-}
destructureTheArgument :
    { argumentAlsoUsedElsewhere :
        OrOnlyCreatingSeparateLetLeft
            DestructureUsingAs
            separateLetAllowed
    }
    ->
        OrOnlyCreatingSeparateLetLeft
            (DestructureTheArgument separateLetAllowed)
            separateLetAllowed
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
            destructureUsingAsInstead
        , ...
        }

-}
destructureUsingAsInstead : ArgumentAlsoUsedElsewhere separateLetAllowed
destructureUsingAsInstead =
    DestructureUsingAsInstead


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

All options:

  - | If no lets exist
      - [`destructureUsingAs`](NoSinglePatternCase#destructureUsingAs)
      - your fix for the situation "only separate let fix left"

For example

    fixByDestructuringTheArgument
        { argumentAlsoUsedElsewhere =
            destructureInExistingLetsInstead
                { noExistingLets = destructureUsingAs }
        , ...
        }

-}
destructureInExistingLetsInstead :
    { noExistingLets :
        OrOnlyCreatingSeparateLetLeft
            DestructureUsingAs
            separateLetAllowed
    }
    -> ArgumentAlsoUsedElsewhere separateLetAllowed
destructureInExistingLetsInstead =
    DestructureInExistingLetsInstead


{-| Create a separate let for this case.
-}
createSeparateLet : OrOnlyCreatingSeparateLetLeft option CreateSeparateLet
createSeparateLet =
    OnlyCreatingSeparateLetLeftFix


{-| Don't create a fix for this case.
-}
noFix : OrOnlyCreatingSeparateLetLeft option DontCreateSeparateLet
noFix =
    OnlyCreatingSeparateLetLeftFix



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
        { argumentAlsoUsedElsewhere = destructureUsingAsInstead
        , notDestructable = noFix
        }
        |> noFixOnNameClash

-}
alwaysFixInArgument : Config DontCreateSeparateLet
alwaysFixInArgument =
    fixByDestructuringTheArgument
        { argumentAlsoUsedElsewhere = destructureUsingAsInstead
        , notDestructable = noFix
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



--


checkDeclaration : Config separateLetAllowed -> Declaration -> List (Error {})
checkDeclaration config declaration =
    case declaration of
        FunctionDeclaration fun ->
            let
                { expression, arguments } =
                    fun.declaration |> Node.value
            in
            expression
                |> checkEpression config
                    { vars =
                        arguments
                            |> List.concatMap allVarsInPattern
                            |> List.map
                                (inScope (expression |> Node.value))
                    , mostInnerLetBlock = Nothing
                    }

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
    Config separateLetAllowed
    ->
        { vars :
            List
                { name : String
                , nameRange : Range
                , kind : VarPatternKind
                , scope : Expression
                }
        , mostInnerLetBlock :
            Maybe
                { declarations : List LetDeclaration
                , expression : Node Expression
                , blockRange : Range
                }
        }
    -> Node Expression
    -> List (Error {})
checkEpression config { vars, mostInnerLetBlock } expressionNode =
    let
        checkExpressionHereWith { extraPatterns, newMostInnerLetBlock } =
            checkEpression config
                { vars =
                    vars
                        ++ (extraPatterns
                                |> List.map
                                    (inScope (Node.value expressionNode))
                           )
                , mostInnerLetBlock =
                    case newMostInnerLetBlock of
                        Just { expression, declarations } ->
                            { blockRange = Node.range expressionNode
                            , expression = expression
                            , declarations = declarations |> List.map Node.value
                            }
                                |> Just

                        Nothing ->
                            mostInnerLetBlock
                }

        checkExpressionHere extraPatterns =
            checkExpressionHereWith
                { extraPatterns = extraPatterns
                , newMostInnerLetBlock = Nothing
                }
    in
    case Node.value expressionNode of
        CaseExpression caseBlock ->
            case caseBlock.cases of
                [ ( singleCasePattern, Node _ singleCaseExpression ) ] ->
                    [ singlePatternCaseError config
                        { patternVars = vars
                        , expressionInCaseOf =
                            caseBlock.expression |> Node.value
                        , singleCasePattern = singleCasePattern
                        , singleCaseExpression = singleCaseExpression
                        , caseRange = Node.range expressionNode
                        , mostInnerLetBlock = mostInnerLetBlock
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

                checkExpressionInThisLetBlock newPatterns =
                    checkExpressionHereWith
                        { extraPatterns = newVarsInLetBlock ++ newPatterns
                        , newMostInnerLetBlock = Just letBlock
                        }

                checkLetDeclaration (Node _ letDeclaration) =
                    case letDeclaration of
                        LetFunction fun ->
                            let
                                declaration =
                                    fun.declaration |> Node.value
                            in
                            declaration.expression
                                |> checkExpressionInThisLetBlock
                                    (declaration.arguments
                                        |> List.concatMap allVarsInPattern
                                    )

                        LetDestructuring _ expr ->
                            expr |> checkExpressionInThisLetBlock []
            in
            letBlock.expression
                |> checkExpressionInThisLetBlock []
                |> (++) checkDeclarations

        otherExpression ->
            expressionsInExpression otherExpression
                |> List.concatMap (checkExpressionHere [])


{-| An error for when a case expression only contains one case pattern. See [`Config`](NoSinglePatternCase#Config) for how fixes will be generated.
-}
singlePatternCaseError :
    Config separateLetAllowed
    ->
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
        , mostInnerLetBlock :
            Maybe
                { declarations : List LetDeclaration
                , expression : Node Expression
                , blockRange : Range
                }
        }
    -> Error {}
singlePatternCaseError (Config fixKind onlySeparateLetFixLeft) information =
    let
        errorInfo =
            { message = "Single pattern case block."
            , details = [ "Single pattern case blocks are either unnecessary or overly verbose.  There's usually a more concise way to destructure, e.g. in a function argument, so consider refactoring." ]
            }

        { patternVars, expressionInCaseOf, singleCaseExpression, caseRange, mostInnerLetBlock } =
            information

        singleCasePatternNode =
            information.singleCasePattern

        singleCasePattern =
            singleCasePatternNode |> Node.value

        fix =
            case fixKind of
                FixByDestructuringInExistingLets { noExistingLets } ->
                    fixInExistingLets
                        { noExistingLets =
                            noExistingLets
                                |> onlyCreatingSeparateLetLeftFixOr
                                    (\(DestructureTheArgument { argumentAlsoUsedElsewhere }) ->
                                        case isDestructurable expressionInCaseOf of
                                            Destructurable varInCaseOf ->
                                                replaceVarPatternFixIfUsedOnce varInCaseOf
                                                    { usedOften =
                                                        argumentAlsoUsedElsewhere
                                                            |> onlyCreatingSeparateLetLeftFixOr
                                                                (\DestructureUsingAs ->
                                                                    destructureUsingAsFix varInCaseOf
                                                                )
                                                    }

                                            NotDestructurable ->
                                                onlySeparateLetFixLeftFix ()
                                    )
                        }

                FixByDestructuringTheArgument { argumentAlsoUsedElsewhere, notDestructable } ->
                    case isDestructurable expressionInCaseOf of
                        Destructurable varInCaseOf ->
                            replaceVarPatternFixIfUsedOnce varInCaseOf
                                { usedOften =
                                    case argumentAlsoUsedElsewhere of
                                        DestructureUsingAsInstead ->
                                            destructureUsingAsFix varInCaseOf

                                        DestructureInExistingLetsInstead { noExistingLets } ->
                                            fixInExistingLets
                                                { noExistingLets =
                                                    noExistingLets
                                                        |> onlyCreatingSeparateLetLeftFixOr
                                                            (\DestructureUsingAs ->
                                                                destructureUsingAsFix varInCaseOf
                                                            )
                                                }
                                }

                        NotDestructurable ->
                            notDestructable
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
                        { varRange = varInCaseOf.nameRange
                        , varPatternScope = varInCaseOf.scope
                        }

                _ ->
                    usedOften

        isDestructurable expression =
            case expression of
                FunctionOrValue [] varName ->
                    case
                        patternVars
                            |> List.filter
                                (.name >> (==) varName)
                    of
                        varPatternInCaseOf :: _ ->
                            case varPatternInCaseOf.kind of
                                SingleVarPattern ->
                                    Destructurable varPatternInCaseOf

                                VarAfterAs ->
                                    NotDestructurable

                                FieldPattern ->
                                    NotDestructurable

                                AnnotatedLetVar ->
                                    NotDestructurable

                        [] ->
                            NotDestructurable

                _ ->
                    NotDestructurable

        fixInExistingLets { noExistingLets } =
            case mostInnerLetBlock of
                Just existingLetBlock ->
                    fixInLetBlock existingLetBlock

                Nothing ->
                    noExistingLets

        onlyCreatingSeparateLetLeftFixOr inFix options =
            case options of
                OnlyCreatingSeparateLetLeftFix ->
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
            case pattern of
                AllPattern ->
                    [ replaceCaseWithExpressionAfterThePattern ]

                UnitPattern ->
                    [ replaceCaseWithExpressionAfterThePattern ]

                _ ->
                    notUseless

        noNameClashIn scope =
            allVarsInPattern singleCasePatternNode
                |> List.all
                    (.name >> usesIn scope >> (==) 1)

        onlySeparateLetFixLeftFixIfNameClashIn scope { noClash } =
            if noNameClashIn scope then
                noClash

            else
                onlySeparateLetFixLeftFix ()

        destructureUsingAsFix { name, nameRange, scope } =
            singleCasePattern
                |> replaceUselessCase
                    { notUseless =
                        onlySeparateLetFixLeftFixIfNameClashIn scope
                            { noClash =
                                [ Fix.replaceRangeBy nameRange
                                    (asPattern singleCasePattern name
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
                                (pattern |> parensAroundNamedPattern)
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
                                    |> updateExpressionsInExpression
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
                                    (singleCasePattern |> parensAroundNamedPattern)
                                    expressionInCaseOf
                                ]
                                singleCaseExpression
                                |> prettyExpressionReplacing caseRange
                            )
                        ]
                    }
    in
    Rule.errorWithFix errorInfo caseRange fix


type IsDestructable
    = Destructurable
        { name : String
        , nameRange : Range
        , kind : VarPatternKind
        , scope : Expression
        }
    | NotDestructurable


prettyExpressionReplacing : Range -> Expression -> String
prettyExpressionReplacing replacedRange =
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
