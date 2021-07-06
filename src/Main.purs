module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Example1.Main as Example1
import Example2.Main as Example2
import Example3.Main as Example3
import Halogen (Component)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Intro.Main as Intro

component :: forall query input output m. MonadEffect m => MonadAff m => Component query input output m
component = Example1.component

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    runUI component unit body
