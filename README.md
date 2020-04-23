[![CircleCI](https://circleci.com/gh/hanshoglund/iso-deriving.svg?style=svg)](https://circleci.com/gh/hanshoglund/iso-deriving)

# iso-deriving

The GHC extension `DerivingVia` allow derivation through a specific morphism:
`coerce`. This library helps generalizing DerivingVia to arbitrary morphisms.
This is particularly useful for prototypes and specifications of instances.

See this [blog post][post] for more details!

[post]: https://www.tweag.io/posts/2020-04-23-deriving-isomorphically.html
