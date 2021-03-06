{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "behaviors"
  , "canvas"
  , "cartesian"
  , "colors"
  , "console"
  , "effect"
  , "either"
  , "event"
  , "foldable-traversable"
  , "halogen"
  , "heterogeneous"
  , "identity"
  , "indexed-monad"
  , "integers"
  , "lists"
  , "math"
  , "maybe"
  , "mezgeb"
  , "painting"
  , "prelude"
  , "psci-support"
  , "record"
  , "simple-json"
  , "sized-vectors"
  , "transformers"
  , "typelevel"
  , "typelevel-peano"
  , "wags"
  , "wags-lib"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
