module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Intro.Main as Intro
import Example1.Main as Example1
import Example2.Main as Example2
import Example3.Main as Example3

data ShowNow = Intr | E1 | E2 | E3

showNow = Intr :: ShowNow

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    runUI (case showNow of
      Intr -> Intro.component
      E1 -> Example1.component
      E2 -> Example2.component
      E3 -> Example3.component) unit body
