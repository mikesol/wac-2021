module Example1.Audio where

import Prelude
import Control.Apply.Indexed ((:*>))
import Control.Comonad.Cofree (head, tail)
import Data.Int (toNumber)
import Data.Lens (_1, over, traversed)
import Data.List (List(..), (..), (:))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (D4, D5)
import Data.Vec ((+>))
import Data.Vec as V
import WAGS.Change (ichange)
import WAGS.Control.Functions.Validated (iloop, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Graph.AudioUnit (OnOff(..), TGain, TPeriodicOsc, TSpeaker)
import WAGS.Graph.Parameter (ff)
import WAGS.Interpret (class AudioInterpret)
import WAGS.Math (calcSlope, calcSlopeExp)
import WAGS.Lib.SFofT (SAPFofT, makePiecewise)
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


type FrameTp a e p i o x
  = IxWAG a e p Unit i o x

envE = 0.11 :: Number

pwf' :: List (Number /\ Number)
pwf' = (0.03 /\ 1.0) : (0.07 /\ 0.1) : (0.09 /\ 0.0) : (envE /\ 0.0) : Nil

pwf :: NonEmpty List (Number /\ Number)
pwf =
  (0.00 /\ 0.0)
    :| (join $ map (\i -> over (traversed <<< _1) (add (envE * toNumber i)) pwf') (0 .. 100))

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

createFrame ::
  forall audio engine.
  AudioInterpret audio engine =>
  FrameTp audio engine Frame0 {} SceneType { volume :: SAPFofT }
createFrame =
  ipatch
    :*> ( ichange
          { mix: 0.1
          , unit0: 0.0
          , unit1: 0.0
          , unit2: 0.0
          , osc0: { waveform: osc0, freq: 220.0, onOff: On }
          , osc1: { waveform: osc1, freq: 440.0, onOff: On }
          , osc2: { waveform: osc2, freq: 880.0, onOff: On }
          }
          $> { volume: makePiecewise pwf }
      )

piece ::
  forall audio engine.
  AudioInterpret audio engine =>
  Scene (SceneI Unit Unit) audio engine Frame0 Unit
piece =
  (const createFrame)
    @!> iloop \e { volume } ->
        let
          { time, headroom } = e

          pulse = volume { time, headroom: toNumber headroom / 1000.0 }

          g' = ff 0.04 $ head pulse

          fade
            | time < 1.0 = g' $> calcSlope 0.0 0.0 1.0 1.0 time
            | otherwise = g' $> 1.0

          g = g' * fade

          ramp = g $> calcSlopeExp 0.0 1.0 10.0 0.0 4.0 time
        in
          ichange
            { unit0: g * ramp
            , unit1: g
            , unit2: g * (sub 1.0 <$> ramp)
            }
            $> { volume: tail pulse }
