module Audio where

import Prelude
import Data.Tuple.Nested (type (/\))
import Data.Vec as V
import WAGS.Control.Functions.Validated (freeze, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Graph.AudioUnit (TGain, TPeriodicOsc, TSpeaker)
import WAGS.Patch (ipatch)
import WAGS.Run (RunAudio, SceneI, RunEngine)

type POsc (a :: Type)
  = V.Vec a Number /\ V.Vec a Number

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
  = IxWAG RunAudio RunEngine p Unit i o a

createFrame :: FrameTp Frame0 {} SceneType Unit
createFrame = ipatch

piece :: Scene (SceneI Unit Unit) RunAudio RunEngine Frame0 Unit
piece = const createFrame @!> freeze
