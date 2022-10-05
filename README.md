# elm-review-no-single-pattern-case

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to disallow single-pattern case expressions.

## Provided rules

* [🔧 `NoSinglePatternCase`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-no-single-pattern-case/2.0.2/NoSinglePatternCase/) - Reports `case...of` expressions that match only a single pattern.  Such case expressions typically are either unnecessary or may be written more concisely.

## Configuration

```elm
module ReviewConfig exposing (config)

import NoSinglePatternCase
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoSinglePatternCase.rule
        NoSinglePatternCase.fixInArgument
    ]
```

## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template SiriusStarr/elm-review-no-single-pattern-case/example/fix-in-argument --rules NoSinglePatternCase
```

## Changelog

* `2.0.2`
  * **New features:**
    * ✨ The rule is now capable of reducing the expressions and patterns of a
      single-pattern case to find the original binding sites to reduce
      expressions at.  The following improvements have been added:
      * Reduce parenthesized expressions:

        ```elm
        unpack o =
            case (o) of
                Opaque i -> i
        -->
        unpack (Opaque i) =
            i
        ```

      * Reduce constructed tupled expressions

        ```elm
        add a b =
            case ( a, b ) of
                ( Opaque i1, Opaque i2 ) ->
                    Opaque <| i1 + i2
        -->
        add (Opaque i1) (Opaque i2) =
            Opaque <| i1 + i2
        ```

      * Reduce constructed record expressions

        ```elm
        pointless a b c =
            case { d = a, e = b, f = c } of
                { d, e, f } -> e
        -->
        pointless _ e _ =
            e
        ```

      * Reduce unnecessary `as` patterns

        ```elm
        unpack o =
            case o of
                ((Opaque i) as a) -> i
        -->
        unpack (Opaque i) =
            i
        ```

      * Reduce function application/named patterns

        ```elm
        multWrap a b =
            case (Tuple a b) of
                Tuple i1 i2 ->
                    Opaque <| i1 * i2
        -->
        multWrap i1 i2 =
            Opaque <| i1 * i2
        ```

    * ✨ With `replaceUnusedBindings`, unit patterns are now fixed to `()`
      instead of `_` to be more explicit.  (There is an
      [`elm-review` rule](https://package.elm-lang.org/packages/mthadley/elm-review-unit/latest/)
      to enforce this in your code, should you so desire.)
    * 🚸 The error range reported by the rule now flags just the pattern,
      instead of the entire case, for less cluttered errors (especially with
      tooling).  No more red-underlining large sections of code!
  * **Bugfixes:**
    * 🐛 Fixed a bug where name clashes were only checked for expressions, not patterns, when generating fixes, leading to the generation of uncompilable code.  This probably rarely happened in practice, but for instance this incorrect fix used to be generated:

        ```elm
        unpack o =
            let
                foo i =
                    0
            in
            case o of
                Opaque i -> i
        -->
        unpack (Opaque i) =
            let
                foo i = -- THIS IS A NAME CLASH FOR `i`
                    0
            in
            i
        ```

    * 🐛 Fixed a bug where both subexpressions of a single-pattern case were not
      actually checked by this rule.  This means that nested single-pattern
      cases are now detected properly (previously, only the outermost one would
      be flagged).  This applies to both those nested following the pattern or within the `case...of`.
  * Documentation changes:
    * 📝 Added a recommendation to add
      [`NoUnused.Patterns`](https://package.elm-lang.org/packages/jfmengels/elm-review-unused/latest/NoUnused-Patterns/)
      to one's config, as the fixes from this rule can sometimes generate nested
      `as` patterns, which will be detected by that rule.
    * 🔥 Removed some references to old v1.0 configs
* `2.0.1` -- Bump `elm-syntax` to v7.2.8, fixing issued with lambda ranges.
  Unlikely to affect this rule in practice, however.
* `2.0.0` -- Add fixes, with option to destructure in the argument or in a `let`
* `1.0.1` -- Fix minor documentation issue
* `1.0.0` -- Initial release
