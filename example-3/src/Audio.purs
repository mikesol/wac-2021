module Audio where

import Prelude
import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..))
import Data.Tuple.Nested (type (/\))
import Data.Vec as V
import Math ((%))
import WAGS.Control.Functions (icont)
import WAGS.Control.Functions.Validated (ibranch, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene, WAG)
import WAGS.Graph.AudioUnit (TBandpass, TGain, TPlayBuf, TSawtoothOsc, TSpeaker)
import WAGS.Patch (ipatch)
import WAGS.Run (RunAudio, SceneI, RunEngine)

type POsc (a :: Type)
  = V.Vec a Number /\ V.Vec a Number

type BaseScene toMix rest
  = { speaker :: TSpeaker /\ { mix :: Unit }
    , mix :: TGain /\ { pad :: Unit | toMix }
    , pad :: TGain /\ { bpf0 :: Unit, bpf1 :: Unit, bpf2 :: Unit, bpf3 :: Unit, bpf4 :: Unit }
    , bpf0 :: TBandpass /\ { osc :: Unit }
    , bpf1 :: TBandpass /\ { osc :: Unit }
    , bpf2 :: TBandpass /\ { osc :: Unit }
    , bpf3 :: TBandpass /\ { osc :: Unit }
    , bpf4 :: TBandpass /\ { osc :: Unit }
    , osc :: TSawtoothOsc /\ {}
    | rest
    }

type BaseSceneClosed
  = BaseScene () ()

type BaseSceneOpen
  = BaseScene ( buf :: Unit ) ( buf :: TPlayBuf /\ {} )

type FrameTp p i o a
  = IxWAG RunAudio RunEngine p Unit i o a

type SceneTp :: forall k. k -> Type
type SceneTp p
  = Scene (SceneI Unit Unit) RunAudio RunEngine p Unit

type ContTp p i a
  = WAG RunAudio RunEngine p Unit i a -> SceneTp p

createFrame :: FrameTp Frame0 {} BaseSceneClosed Unit
createFrame = ipatch

step2 :: forall proof. ContTp proof BaseSceneOpen Unit
step2 =
  ibranch \e _ ->
    if e.time % 10.0 > 5.0 then
      Right $ ipure unit
    else
      Left
        $ icont step1 Ix.do
            ipatch

step1 :: forall proof. ContTp proof BaseSceneClosed Unit
step1 =
  ibranch \e _ ->
    if e.time % 10.0 < 5.0 then
      Right $ ipure unit
    else
      Left
        $ icont step2 Ix.do
            ipatch

piece :: SceneTp Frame0
piece = const createFrame @!> step1
