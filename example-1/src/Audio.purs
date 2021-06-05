module Audio where

import Prelude

import Control.Applicative.Indexed (ivoid)
import Control.Apply.Indexed ((:*>))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (D4, D5)
import Data.Vec ((+>))
import Data.Vec as V
import Effect (Effect)
import WAGS.Change (ichange)
import WAGS.Control.Functions.Validated (freeze, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Graph.AudioUnit (TGain, TPeriodicOsc, TSpeaker)
import WAGS.Graph.Optionals (gain_, periodicOsc_)
import WAGS.Interpret (FFIAudio)
import WAGS.Patch (ipatch)
import WAGS.Run (SceneI)

type POsc (a :: Type)
  = V.Vec a Number /\ V.Vec a Number

osc0 :: POsc D4
osc0 = (0.0 +> 0.2 +> -0.1 +> 0.05 +> V.empty) /\ (0.0 +> 0.02 +> 0.03 +> 0.1 +> V.empty)

osc1 :: POsc D4
osc1 = (0.0 +> 0.03 +> 0.05 +> 0.1 +> V.empty) /\ (0.0 +> 0.1 +> -0.2 +> 0.03 +> V.empty)

osc2 :: POsc D5
osc2 = (0.0 +> 0.01 +> -0.2 +> -0.1 +> 0.05 +> V.empty) /\ (0.0 +> 0.01 +> 0.02 +> 0.2 +> 0.01 +> V.empty)

type SceneType
  = { speaker :: TSpeaker /\ { mix :: Unit }
    , mix :: TGain /\ { unit0 :: Unit, unit1 :: Unit, unit2 :: Unit }
    , unit0 :: TGain /\ { osc0 :: Unit }
    , osc0 :: TPeriodicOsc /\ {}
    , unit1 :: TGain /\ { osc1 :: Unit }
    , osc1 :: TPeriodicOsc /\ {}
    , unit2 :: TGain /\ { osc2 :: Unit }
    , osc2 :: TPeriodicOsc /\ {}
    }

type FrameTp p i o a
  = IxWAG FFIAudio (Effect Unit) p Unit i o a

createFrame :: FrameTp Frame0 {} SceneType Unit
createFrame =
  ipatch
    :*> ( ivoid
          $ ichange
              { mix: gain_ 0.1
              , unit0: gain_ 0.1
              , unit1: gain_ 0.1
              , unit2: gain_ 0.1
              , osc0: periodicOsc_ osc0 220.0
              , osc1: periodicOsc_ osc1 440.0
              , osc2: periodicOsc_ osc2 880.0
              }
      )

piece :: Scene (SceneI Unit Unit) FFIAudio (Effect Unit) Frame0 Unit
piece =
  (const createFrame)
    @!> freeze
