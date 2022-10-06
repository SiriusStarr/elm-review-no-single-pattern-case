module Util exposing
    ( Binding
    , Destructuring
    , Either(..)
    , bindingsInPattern
    , either3
    , nameClash
    , nameUsedOutsideExprs
    , namesUsedInExpression
    , reduceDestructuring
    , reindent
    , subexpressions
    )

{-| General utility functions not directly related to the specific rule.
-}

import Dict exposing (Dict)
import Dict.Extra as DictX
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..), RecordSetter)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import List.Extra as ListX
import Maybe.Extra as MaybeX
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Set exposing (Set)


{-| Get all immediate child expressions of an expression.
-}
subexpressions : Node Expression -> List (Node Expression)
subexpressions e =
    case Node.value e of
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
            letBlock.expression
                :: List.map subExprs letBlock.declarations

        ListExpr exprs ->
            exprs

        TupledExpression exprs ->
            exprs

        RecordExpr setters ->
            List.map (Tuple.second << Node.value) setters

        RecordUpdateExpression record updaters ->
            Node.map (FunctionOrValue []) record
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


{-| Get a set of all (unqualified) bindings used in an expression.
-}
namesUsedInExpression : Node Expression -> Set String
namesUsedInExpression expr =
    case Node.value expr of
        -- If the name is qualified, it isn't a variable
        FunctionOrValue [] n ->
            Set.singleton n

        _ ->
            subexpressions expr
                -- Map and accumulate in one pass
                |> List.foldl (\e -> Set.union (namesUsedInExpression e)) Set.empty


{-| `Destructuring` represents a fully-reduced case expression.

  - `removableBindings` -- These are bindings that are used in removable parts of
    the `case...of` expression but nowhere else and can thus be safely replaced
    with `_`.
  - `usefulPatterns` -- These are actually necessery patterns (i.e. those that
    bind names used in the output expression), along with the expression they
    directly destructure.
  - `removedExpressions` -- These are expressions that will be removed when the
    `case` is rewritten, so we can ignore the use of anything in them.
  - `ignoredPatterns` -- Patterns that we were told to not reduce, so they will
    be left as a `case...of`.

-}
type alias Destructuring =
    { removableBindings : List { isUnit : Bool, binding : Binding }
    , usefulPatterns : List ( Node Pattern, Node Expression )
    , removedExpressions : List (Node Expression)
    , ignoredPatterns : List ( Node Pattern, Node Expression )
    }


{-| Given a pattern and the expression it destructures, reduce them to their
simplest forms.
-}
reduceDestructuring :
    { bindings : Dict String Binding
    , lookupTable : ModuleNameLookupTable
    , outputExpression : Node Expression
    }
    -> Node Pattern
    -> Node Expression
    -> Destructuring
reduceDestructuring { bindings, outputExpression, lookupTable } pattern expression =
    pairPatternsWithExpression lookupTable (namesUsedInExpression outputExpression) pattern expression
        |> (\{ uselessPatterns, usefulPatterns, uselessExpressions, ignoredPatterns } ->
                let
                    allUselessExpressions : List (Node Expression)
                    allUselessExpressions =
                        List.map .expression uselessPatterns ++ uselessExpressions
                in
                { removableBindings = removableBindings bindings allUselessExpressions uselessPatterns
                , usefulPatterns = usefulPatterns
                , removedExpressions = allUselessExpressions
                , ignoredPatterns = ignoredPatterns
                }
           )


{-| Given a lookup table and a set of useful bound names, fully-pair patterns and
expressions.
-}
pairPatternsWithExpression :
    ModuleNameLookupTable
    -> Set String
    -> Node Pattern
    -> Node Expression
    ->
        { uselessPatterns : List { isUnit : Bool, pattern : Node Pattern, expression : Node Expression }
        , usefulPatterns : List ( Node Pattern, Node Expression )
        , uselessExpressions : List (Node Expression)
        , ignoredPatterns : List ( Node Pattern, Node Expression )
        }
pairPatternsWithExpression lookupTable usedNames pat expr =
    let
        go :
            Node Pattern
            -> Node Expression
            ->
                { uselessPatterns : List { isUnit : Bool, pattern : Node Pattern, expression : Node Expression }
                , usefulPatterns : List ( Node Pattern, Node Expression )
                , uselessExpressions : List (Node Expression)
                , ignoredPatterns : List ( Node Pattern, Node Expression )
                }
        go =
            pairPatternsWithExpression lookupTable usedNames

        goMultiple :
            List (Node Pattern)
            -> List (Node Expression)
            ->
                { uselessPatterns : List { isUnit : Bool, pattern : Node Pattern, expression : Node Expression }
                , usefulPatterns : List ( Node Pattern, Node Expression )
                , uselessExpressions : List (Node Expression)
                , ignoredPatterns : List ( Node Pattern, Node Expression )
                }
        goMultiple ps es =
            List.map2 go ps es
                |> List.foldr
                    (\{ uselessPatterns, usefulPatterns, uselessExpressions, ignoredPatterns } acc ->
                        { uselessPatterns = uselessPatterns ++ acc.uselessPatterns
                        , usefulPatterns = usefulPatterns ++ acc.usefulPatterns
                        , uselessExpressions = uselessExpressions ++ acc.uselessExpressions
                        , ignoredPatterns = ignoredPatterns ++ acc.ignoredPatterns
                        }
                    )
                    { uselessPatterns = []
                    , usefulPatterns = []
                    , uselessExpressions = []
                    , ignoredPatterns = []
                    }

        done :
            Bool
            ->
                { uselessPatterns : List { isUnit : Bool, pattern : Node Pattern, expression : Node Expression }
                , usefulPatterns : List ( Node Pattern, Node Expression )
                , uselessExpressions : List (Node Expression)
                , ignoredPatterns : List ( Node Pattern, Node Expression )
                }
        done isUnit =
            if isUseful pat then
                -- Not useless
                { uselessPatterns = []
                , usefulPatterns = [ ( pat, expr ) ]
                , uselessExpressions = []
                , ignoredPatterns = []
                }

            else
                -- Useless
                { uselessPatterns = [ { isUnit = isUnit, pattern = pat, expression = expr } ]
                , usefulPatterns = []
                , uselessExpressions = []
                , ignoredPatterns = []
                }

        isUseful : Node Pattern -> Bool
        isUseful p =
            namesInPattern p
                |> List.any (\( name, _ ) -> Set.member name usedNames)
    in
    case ( Node.value pat, Node.value expr ) of
        ( ParenthesizedPattern p, _ ) ->
            -- Recurse if either parenthesized
            go p expr

        ( _, ParenthesizedExpression e ) ->
            -- Recurse if either parenthesized
            go pat e

        ( AllPattern, _ ) ->
            -- Get the binding info if it's a terminal pattern
            done False

        ( UnitPattern, _ ) ->
            -- Get the binding info if it's a terminal pattern
            done True

        ( VarPattern _, _ ) ->
            -- Get the binding info if it's a terminal pattern
            done False

        ( CharPattern _, _ ) ->
            -- Get the binding info if it's a terminal pattern
            done False

        ( StringPattern _, _ ) ->
            -- Get the binding info if it's a terminal pattern
            done False

        ( IntPattern _, _ ) ->
            -- Get the binding info if it's a terminal pattern
            done False

        ( HexPattern _, _ ) ->
            -- Get the binding info if it's a terminal pattern
            done False

        ( FloatPattern _, _ ) ->
            -- Get the binding info if it's a terminal pattern
            done False

        ( _, UnitExpr ) ->
            -- Get the binding info if it's a terminal expression
            done True

        ( _, Integer _ ) ->
            -- Get the binding info if it's a terminal expression
            done False

        ( _, Hex _ ) ->
            -- Get the binding info if it's a terminal expression
            done False

        ( _, Floatable _ ) ->
            -- Get the binding info if it's a terminal expression
            done False

        ( _, Operator _ ) ->
            -- Get the binding info if it's a terminal expression
            done False

        ( _, CharLiteral _ ) ->
            -- Get the binding info if it's a terminal expression
            done False

        ( _, GLSLExpression _ ) ->
            -- Get the binding info if it's a terminal expression
            done False

        ( _, Literal _ ) ->
            -- Get the binding info if it's a terminal expression
            done False

        ( _, LambdaExpression _ ) ->
            -- Get the binding info if it's a terminal expression
            done False

        ( UnConsPattern _ _, _ ) ->
            -- An uncons pattern can't occur in a single-pattern case,
            -- since there would have to always be at least the `[]`
            -- pattern as well.
            done False

        ( ListPattern _, _ ) ->
            -- A list pattern can't occur in a single-pattern case,
            -- since there would have to always be an infinite length
            -- pattern as well.
            done False

        ( AsPattern p n, _ ) ->
            if Set.member (Node.value n) usedNames then
                -- As pattern is used, so cannot destructure further
                done False

            else
                -- As pattern is useless, so recurse
                go p expr

        ( TuplePattern ps, TupledExpression es ) ->
            goMultiple ps es

        ( TuplePattern _, _ ) ->
            -- It's not worth it to try to reduce the few other cases we
            -- potentially could here, e.g. `let...in`, because they
            -- should almost never show up
            done False

        ( NamedPattern _ [], _ ) ->
            -- No sub-patterns, so this is terminal
            done False

        ( NamedPattern { name } ps, Application exprs ) ->
            ListX.uncons exprs
                |> MaybeX.unpack (\() -> done False)
                    (\( e, es ) ->
                        case Node.value e of
                            FunctionOrValue _ eName ->
                                if
                                    -- Confirm they are the same name and from the same module
                                    (name == eName)
                                        && (Maybe.map2 (==)
                                                (ModuleNameLookupTable.moduleNameFor lookupTable pat)
                                                (ModuleNameLookupTable.moduleNameFor lookupTable e)
                                                |> Maybe.withDefault False
                                           )
                                then
                                    goMultiple ps es

                                else
                                    -- Names aren't a match
                                    done False

                            _ ->
                                -- It's not worth it to try to reduce the few other cases we
                                -- potentially could here, e.g. `let...in`, because they
                                -- should almost never show up
                                done False
                    )

        ( NamedPattern _ _, _ ) ->
            -- It's not worth it to try to reduce the few other cases we
            -- potentially could here, e.g. `let...in`, because they
            -- should almost never show up
            done False

        ( RecordPattern ps, RecordExpr es ) ->
            let
                { used, unused } =
                    pairRecordDestructuring ps es

                ( usefulPatterns, uselessPatterns ) =
                    -- Convert `Node String` to `Node Pattern`, since `RecordPattern` is weird
                    List.map (Tuple.mapBoth (Node.map VarPattern) (Tuple.second << Node.value)) used
                        |> List.partition (isUseful << Tuple.first)
            in
            { uselessPatterns = List.map (\( p, e ) -> { isUnit = False, pattern = p, expression = e }) uselessPatterns
            , usefulPatterns = usefulPatterns
            , uselessExpressions = List.map (Tuple.second << Node.value) unused
            }

        ( RecordPattern _, _ ) ->
            -- It's not worth it to try to reduce the few other cases we
            -- potentially could here, e.g. record update, because they
            -- should almost never show up
            done False


{-| Given a record pattern and a record literal, return paired patterns and
expressions, as well as any useless setters.
-}
pairRecordDestructuring : List (Node String) -> List (Node RecordSetter) -> { used : List ( Node String, Node RecordSetter ), unused : List (Node RecordSetter) }
pairRecordDestructuring ps es =
    List.foldr
        (\setter ( usedAcc, unusedAcc, remaining ) ->
            let
                name : String
                name =
                    Node.value <| Tuple.first <| Node.value setter
            in
            case List.partition (\p -> name == Node.value p) remaining of
                ( [ p ], rem ) ->
                    ( ( p, setter ) :: usedAcc, unusedAcc, rem )

                ( _, rem ) ->
                    ( usedAcc, setter :: unusedAcc, rem )
        )
        ( [], [], ps )
        es
        |> (\( used, unused, _ ) ->
                -- Throw out any remaining patterns, since they'd be a type error anyways
                { used = used, unused = unused }
           )


{-| Given bindings in scope, a list of ignorable expressions, and a list of
useless patterns, return any binding that are removable (not used elsewhere).
-}
removableBindings : Dict String Binding -> List (Node Expression) -> List { isUnit : Bool, pattern : Node Pattern, expression : Node Expression } -> List { isUnit : Bool, binding : Binding }
removableBindings bindings allUselessExpressions =
    List.concatMap
        (\r ->
            namesUsedInExpression r.expression
                |> (\bs -> DictX.keepOnly bs bindings)
                |> Dict.filter
                    (\name { canDestructureAt, scope } ->
                        canDestructureAt
                            && (countUsesIn scope name == List.foldl (\e acc -> acc + countUsesIn e name) 0 allUselessExpressions)
                    )
                |> Dict.values
                |> List.map (\b -> { isUnit = r.isUnit, binding = b })
        )


{-| Combine two dictionaries. If there is a collision, a combining function is
used to combine the two values.

Note that, like `Dict.union`, it is more efficient to have the larger `Dict` as
the second argument, i.e. when possible, you should use `unionWith f new old`,
if `old` has more keys than `new`.

-}
dictUnionWith : (a -> a -> a) -> Dict comparable a -> Dict comparable a -> Dict comparable a
dictUnionWith f d1 d2 =
    Dict.foldl
        (\k v1 acc ->
            case Dict.get k acc of
                Just v2 ->
                    Dict.insert k (f v1 v2) acc

                Nothing ->
                    Dict.insert k v1 acc
        )
        d2
        d1


{-| Get a count for all names that are bound in a pattern, and the number of
times they are bound (which is always 1, but it's returned this way for ease of
combining with counts from expressions).
-}
nameAppearancesInPattern : Node Pattern -> Dict String Int
nameAppearancesInPattern p =
    -- Add pattern bindings, assuming names can't be bound more than once within a pattern (since they can't)
    namesInPattern p
        |> List.map (Tuple.mapSecond (always 1))
        |> Dict.fromList


{-| Given an expression, count every unqualified name that is bound or
referenced in it and the number of times it is bound and/or referenced.
-}
nameAppearancesInExpression : Node Expression -> Dict String Int
nameAppearancesInExpression expressionNode =
    let
        go : Node Expression -> Dict String Int
        go =
            nameAppearancesInExpression
    in
    case Node.value expressionNode of
        -- If the name is qualified, it isn't a variable
        FunctionOrValue [] n ->
            Dict.singleton n 1

        CaseExpression { cases, expression } ->
            List.map
                (\( p, e ) ->
                    nameAppearancesInPattern p
                        |> dictUnionWith (+) (go e)
                )
                cases
                |> List.foldl (dictUnionWith (+)) (go expression)

        LetExpression lB ->
            let
                bindingsFromDec : Node LetDeclaration -> Dict String Int
                bindingsFromDec d =
                    case Node.value d of
                        LetFunction fun ->
                            let
                                { name, expression, arguments } =
                                    Node.value fun.declaration
                            in
                            List.map nameAppearancesInPattern arguments
                                |> List.foldl (dictUnionWith (+)) (Dict.singleton (Node.value name) 1)
                                |> dictUnionWith (+) (go expression)

                        LetDestructuring pattern expr ->
                            nameAppearancesInPattern pattern
                                |> dictUnionWith (+) (go expr)
            in
            List.foldl (dictUnionWith (+) << bindingsFromDec) (go lB.expression) lB.declarations

        LambdaExpression { args, expression } ->
            List.map nameAppearancesInPattern args
                |> List.foldl (dictUnionWith (+)) (go expression)

        _ ->
            subexpressions expressionNode
                |> List.foldl (\e -> dictUnionWith (+) (go e)) Dict.empty


{-| Recursively find all names used in a pattern and save whether or not
destructuring could occur at the pattern. Requires scope information to actually
create the binding.
-}
namesInPattern : Node Pattern -> List ( String, Node Expression -> Binding )
namesInPattern pattern =
    let
        go : List (Node Pattern) -> List ( String, Node Expression -> Binding )
        go =
            List.concatMap namesInPattern

        makeBinding : Bool -> Node String -> ( String, Node Expression -> Binding )
        makeBinding canDestructureAt name =
            ( Node.value name
            , \scope ->
                { patternNodeRange = Node.range name
                , canDestructureAt = canDestructureAt
                , scope = scope
                }
            )
    in
    case Node.value pattern of
        ListPattern ps ->
            go ps

        TuplePattern ps ->
            go ps

        RecordPattern ps ->
            List.map (makeBinding False) ps

        NamedPattern _ ps ->
            go ps

        UnConsPattern p ps ->
            go [ p, ps ]

        VarPattern name ->
            [ ( name
              , \scope ->
                    { patternNodeRange = Node.range pattern
                    , canDestructureAt = True
                    , scope = scope
                    }
              )
            ]

        AsPattern p name ->
            makeBinding False name :: go [ p ]

        ParenthesizedPattern p ->
            go [ p ]

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


{-| Recursively find all bindings in a pattern and save whether or not
destructuring could occur at the pattern.
-}
bindingsInPattern : Node Expression -> Node Pattern -> List ( String, Binding )
bindingsInPattern scope =
    namesInPattern
        >> List.map (Tuple.mapSecond (\f -> f scope))


{-| Count the uses of a given name in the scope of an expression.
-}
countUsesIn : Node Expression -> String -> Int
countUsesIn expr name =
    case Node.value expr of
        -- If the name is qualified, it isn't a variable
        FunctionOrValue [] n ->
            if n == name then
                1

            else
                0

        _ ->
            subexpressions expr
                -- Count and sum in one pass
                |> List.foldl (\e -> (+) (countUsesIn e name)) 0


{-| Given a name, some number of inner expression, and an outer expression,
report if the name is used in the outer expression exclusive of the inner
expressions.
-}
nameUsedOutsideExprs : { inside : List (Node Expression), scope : Node Expression } -> String -> Bool
nameUsedOutsideExprs { inside, scope } name =
    countUsesIn scope name > List.foldl (\e acc -> acc + countUsesIn e name) 0 inside


{-| Given some number of inner expression and an outer expression, and an inner
pattern, report all names bound in the pattern that are used in the outer
expression exclusive of the inner expressions or the patterns themselves.
-}
nameClash : { insideExpr : List (Node Expression), scope : Node Expression } -> Node Pattern -> Bool
nameClash { insideExpr, scope } ps =
    let
        innerUses : Dict String Int
        innerUses =
            List.map nameAppearancesInExpression insideExpr
                |> List.foldl (dictUnionWith (+)) patternUses

        patternUses : Dict String Int
        patternUses =
            nameAppearancesInPattern ps

        outerUses : Dict String Int
        outerUses =
            nameAppearancesInExpression scope
    in
    DictX.any
        (\n _ ->
            Maybe.map2
                (\outerCount innerCount ->
                    outerCount > innerCount
                )
                (Dict.get n outerUses)
                (Dict.get n innerUses)
                |> Maybe.withDefault False
        )
        patternUses


{-| A binding with some scope.

  - `patternNodeRange` -- The `Range` for the binding location.
  - `canDestructureAt` -- Whether or not the binding can be destructured at.
  - `scope` -- The scope the binding is bound within.

-}
type alias Binding =
    { patternNodeRange : Range
    , canDestructureAt : Bool
    , scope : Node Expression
    }


{-| Re-indent a section of generated code to ensure that it doesn't cause issues
when used as a fix.
-}
reindent : Int -> String -> String
reindent amount =
    let
        indent : String
        indent =
            String.repeat (amount - 1) " "
    in
    String.lines
        >> List.map
            (\l ->
                -- Don't indent empty lines
                if String.isEmpty l then
                    l

                else
                    indent ++ l
            )
        >> String.join "\n"
        >> String.trimLeft


{-| Allow a choice between two options.
-}
type Either a b
    = A a
    | B b


{-| Case analysis for the `Either` type.
-}
either : (a -> c) -> (b -> c) -> Either a b -> c
either fA fB aOrB =
    case aOrB of
        A a ->
            fA a

        B b ->
            fB b


{-| Case analysis for nested `Either` type.
-}
either3 : Either (Either a b) c -> (a -> out) -> (b -> out) -> (c -> out) -> out
either3 abc a b c =
    either (either a b) c abc
