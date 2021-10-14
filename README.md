# elm-review-no-single-pattern-case

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to disallow single-pattern case expressions.

## Provided rules

* [🔧 `NoSinglePatternCase`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-no-single-pattern-case/2.0.1/NoSinglePatternCase/) - Reports `case...of` expressions that match only a single pattern.  Such case expressions typically are either unnecessary or may be written more concisely.

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
elm-review --template SiriusStarr/elm-review-no-single-pattern-case/example
```

## Changelog

* 2.0.1 -- Bump `elm-syntax` to v7.2.8, fixing issued with lambda ranges.
  Unlikely to affect this rule in practice, however.
* 2.0.0 -- Add fixes, with option to destructure in the argument or in a `let`
* 1.0.1 -- Fix minor documentation issue
* 1.0.0 -- Initial release
