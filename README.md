# elm-review-no-single-pattern-case

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to REPLACEME.


## Provided rules

- [`NoSinglePatternCase`](https://package.elm-lang.org/packages/SiriusStarr/elm-review-no-single-pattern-case/1.0.0/NoSinglePatternCase) - Reports REPLACEME.


## Configuration

```elm
module ReviewConfig exposing (config)

import NoSinglePatternCase
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoSinglePatternCase.rule
    ]
```


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template SiriusStarr/elm-review-no-single-pattern-case/example
```
