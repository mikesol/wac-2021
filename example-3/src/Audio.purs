module Audio where

import Prelude
import Control.Applicative.Indexed (ipure)
import Control.Comonad.Cofree (Cofree)
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|), NonEmpty)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Data.Vec as V
import Math ((%))
import WAGS.Control.Functions (icont)
import WAGS.Control.Functions.Validated (ibranch, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene, WAG)
import WAGS.Graph.AudioUnit (TBandpass, TGain, TPlayBuf, TSawtoothOsc, TSpeaker)
import WAGS.NE2CF (nonEmptyToCofree')
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

type NonEmptyToCofree' a
  = Number -> Cofree ((->) Number) a

shift :: Maybe Int -> Int
shift Nothing = 0

shift (Just a) = 1 + a

spacedNE2CF :: forall a. Number -> NonEmpty List a -> NonEmptyToCofree' a
spacedNE2CF space l =
  nonEmptyToCofree' Nothing
    $ mapWithIndex
        ( \i' ->
            let
              i = shift i'

              sp = toNumber i * space

              spNext = toNumber i * space
            in
              Tuple (if i /= NEL.length (NonEmptyList l) - 1 then (_ < spNext) else (_ >= sp))
        )
        l

type Step1Acc
  = { bpf0 :: NonEmptyToCofree' Number
    , bpf1 :: NonEmptyToCofree' Number
    , bpf2 :: NonEmptyToCofree' Number
    , bpf3 :: NonEmptyToCofree' Number
    , bpf4 :: NonEmptyToCofree' Number
    }

initialStep1Acc :: Step1Acc
initialStep1Acc =
  { bpf0: spacedNE2CF 0.2 (1.0 :| 1.5 : 2.0 : 1.5 : 1.0 : 0.5 : 1.0 : Nil)
  , bpf1: spacedNE2CF 0.4 (3.0 :| 4.0 : 5.0 : Nil)
  , bpf2: spacedNE2CF 0.5 (5.0 :| 5.1 : 5.2 : 5.3 : 7.0 : Nil)
  , bpf3: spacedNE2CF 0.15 (8.0 :| 9.0 : Nil)
  , bpf4: spacedNE2CF 0.7 (10.0 :| 10.2 : 10.6 : 10.3 : 11.8 : 13.1 : 12.5 : 16.4 : 9.3 : 14.6 : Nil)
  }

type Step2Acc
  = { osc :: NonEmptyToCofree' Number
    }

initialStep2Acc :: Step2Acc
initialStep2Acc = { osc: spacedNE2CF 0.2 (30.0 :| 35.0 : 31.0 : 34.0 : 39.0 : 37.0 : 32.0 : 33.0 : Nil) }

type BaseSceneClosed
  = BaseScene () ()

type BaseSceneOpen
  = BaseScene ( buf :: Unit ) ( buf :: TPlayBuf /\ {} )

type FrameTp p i o a
  = IxWAG RunAudio RunEngine p Unit i o a

-- type SceneTp :: forall k. k -> Type
type SceneTp p
  = Scene (SceneI Unit Unit) RunAudio RunEngine p Unit

type ContTp p i a
  = WAG RunAudio RunEngine p Unit i a -> SceneTp p

createFrame :: FrameTp Frame0 {} BaseSceneClosed Step1Acc
createFrame = ipatch $> initialStep1Acc

step2 :: forall proof. ContTp proof BaseSceneOpen Step2Acc
step2 =
  ibranch \e a ->
    if e.time % 10.0 > 5.0 then
      Right $ ipure a
    else
      Left
        $ icont step1 (ipatch $> initialStep1Acc)

step1 :: forall proof. ContTp proof BaseSceneClosed Step1Acc
step1 =
  ibranch \e a ->
    if e.time % 10.0 < 5.0 then
      Right $ ipure a
    else
      Left
        $ icont step2 (ipatch $> initialStep2Acc)

piece :: SceneTp Frame0
piece = const createFrame @!> step1
