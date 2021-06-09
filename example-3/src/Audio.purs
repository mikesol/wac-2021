module Audio where

import Prelude

import Control.Applicative.Indexed ((:*>))
import Control.Comonad.Cofree (Cofree, head, tail)
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|), NonEmpty)
import Data.Profunctor (lcmap)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Data.Vec as V
import Heterogeneous.Mapping (hmap)
import Math (pi, pow, sin, (%))
import Record as R
import WAGS.Change (ichange)
import WAGS.Control.Functions (icont)
import WAGS.Control.Functions.Validated (ibranch, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene, WAG)
import WAGS.Graph.AudioUnit (OnOff(..), TBandpass, TGain, TLoopBuf, TSawtoothOsc, TSpeaker)
import WAGS.Interpret (class AudioInterpret)
import WAGS.NE2CF (NonEmptyToCofree', nonEmptyToCofree')
import WAGS.Patch (ipatch)
import WAGS.Rendered (Instruction)
import WAGS.Run (SceneI)

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

              spNext = toNumber (i + 1) * space

              terminus = space * (toNumber $ NEL.length (NonEmptyList l))
            in
              Tuple
                ( lcmap (_ % terminus)
                    (if i /= NEL.length (NonEmptyList l) - 1 then (_ < spNext) else (_ >= sp))
                )
        )
        l

type Step1Acc' o
  = { bpf0 :: o
    , bpf1 :: o
    , bpf2 :: o
    , bpf3 :: o
    , bpf4 :: o
    }

type Step1Acc
  = Step1Acc' (NonEmptyToCofree' Number)

initialStep1Acc :: Step1Acc
initialStep1Acc =
  { bpf0: spacedNE2CF 0.2 (1.0 :| 1.5 : 2.0 : 1.5 : 1.0 : 0.5 : 1.0 : Nil)
  , bpf1: spacedNE2CF 0.4 (3.0 :| 4.0 : 5.0 : Nil)
  , bpf2: spacedNE2CF 0.5 (5.0 :| 5.1 : 5.2 : 5.3 : 7.0 : Nil)
  , bpf3: spacedNE2CF 0.15 (8.0 :| 9.0 : Nil)
  , bpf4: spacedNE2CF 0.7 (10.0 :| 10.2 : 10.6 : 10.3 : 11.8 : 13.1 : 12.5 : 16.4 : 9.3 : 14.6 : Nil)
  }

type Step2Acc' o
  = { osc :: o
    }

type Step2Acc
  = Step2Acc' (NonEmptyToCofree' Number)

initialStep2Acc :: Step2Acc
initialStep2Acc = { osc: spacedNE2CF 0.2 (42.0 :| 47.0 : 43.0 : 46.0 : 51.0 : 49.0 : 44.0 : 45.0 : Nil) }

type BaseSceneClosed
  = BaseScene () ()

type BaseSceneOpen
  = BaseScene ( buf :: Unit ) ( buf :: TLoopBuf /\ {} )

type FrameTp a e p i o x
  = IxWAG a e p Unit i o x

type SceneTp :: forall k. Type -> Type -> k -> Type
type SceneTp a e p
  = Scene (SceneI Unit Unit) a e p Unit

type ContTp a e p i x
  = WAG a e p Unit i x -> SceneTp a e p

createFrame :: forall audio engine.
  AudioInterpret audio engine =>
  FrameTp audio engine Frame0 {} BaseSceneClosed Step1Acc
createFrame =
  ipatch
    :*> ichange
        { mix: 0.1
        , pad: 1.0
        , osc: { freq: fund, onOff: On }
        , bpf0: { q: 40.0 }
        , bpf1: { q: 40.0 }
        , bpf2: { q: 40.0 }
        , bpf3: { q: 40.0 }
        , bpf4: { q: 40.0 }
        }
    $> initialStep1Acc

type CFAp
  = (Number -> (Cofree ((->) Number) Number)) -> (Cofree ((->) Number) Number)

type CFTail
  = Cofree ((->) Number) Number -> Number -> Cofree ((->) Number) Number

type CFHead
  = Cofree ((->) Number) Number -> Number

midi2cps :: Number -> Number
midi2cps i = 440.0 * (2.0 `pow` ((i - 69.0) / 12.0))

step2 :: forall audio engine proof.
  AudioInterpret audio engine =>
  ContTp audio engine proof BaseSceneOpen Step2Acc
step2 =
  ibranch \e a ->
    let
      loopStart = 1.0 + 1.0 * sin (3.0 * e.time * pi)

      actualized = hmap (((#) e.time) :: CFAp) a

      heads = hmap ((midi2cps <<< head) :: CFHead) actualized
    in
      if e.time % 10.0 > 5.0 then
        Right
          $ ichange
              ( R.union
                  { buf:
                      { loopStart
                      , loopEnd: loopStart + 0.4
                      }
                  }
                  heads
              )
          $> (hmap (tail :: CFTail) actualized)
      else
        Left
          $ icont step1 (ipatch :*> ichange { osc: fund } $> initialStep1Acc)

fund = 123.0 :: Number

step1 :: forall audio engine proof.
  AudioInterpret audio engine =>
  ContTp audio engine proof BaseSceneClosed Step1Acc
step1 =
  ibranch \e a ->
    let
      actualized = hmap (((#) e.time) :: CFAp) a

      heads = hmap (((_ * fund) <<< head) :: CFHead) actualized
    in
      if e.time % 10.0 < 5.0 then
        Right $ ichange heads $> (hmap (tail :: CFTail) actualized)
      else
        Left
          $ icont step2
              ( ipatch
                  :*> ichange
                      { bpf0: 1000.0
                      , bpf1: 1400.0
                      , bpf2: 2000.0
                      , bpf3: 2400.0
                      , bpf4: 3000.0
                      , buf: { buffer: "chaos", loopStart: 2.0, loopEnd: 2.2, onOff: On }
                      }
                  $> initialStep2Acc
              )

type PieceRepl = Scene (SceneI Unit Unit) Unit Instruction Frame0 Unit

pieceRepl :: SceneI Unit Unit
pieceRepl = 
  { trigger :unit
  , world :unit
  , time :0.0
  , sysTime : Milliseconds 0.0
  , active : false
  , headroom : 10
  }

piece :: forall audio engine.
  AudioInterpret audio engine =>
  SceneTp audio engine Frame0
piece = const createFrame @!> step1

