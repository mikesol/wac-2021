module Audio where

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
import Effect (Effect)
import WAGS.Change (ichange)
import WAGS.Control.Functions.Validated (iloop, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Graph.AudioUnit (TGain, TPeriodicOsc, TSpeaker)
import WAGS.Graph.Optionals (gain_, periodicOsc_)
import WAGS.Graph.Parameter (ff)
import WAGS.Interpret (FFIAudio)
import WAGS.Math (calcSlope, calcSlopeExp)
import WAGS.NE2CF (ASDR, makePiecewise)
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

jnel :: forall a. NonEmpty List (NonEmpty List a) -> NonEmpty List a
jnel (a :| Nil) = a

jnel ((a :| b) :| ((c :| d) : e)) = jnel ((a :| (b <> pure c <> d)) :| e)

pwf' :: NonEmpty List (Number /\ Number)
pwf' = (0.0 /\ 0.0) :| (0.03 /\ 1.0) : (0.07 /\ 0.1) : (0.09 /\ 0.0) : Nil

pwf :: NonEmpty List (Number /\ Number)
pwf = jnel $ map (\i -> over (traversed <<< _1) (add (0.11 * toNumber i)) pwf') (0 :| (1 .. 100))

createFrame :: FrameTp Frame0 {} SceneType { asdr :: ASDR }
createFrame =
  ipatch
    :*> ( ichange
          { mix: gain_ 0.1
          , unit0: gain_ 0.0
          , unit1: gain_ 0.0
          , unit2: gain_ 0.0
          , osc0: periodicOsc_ osc0 220.0
          , osc1: periodicOsc_ osc1 440.0
          , osc2: periodicOsc_ osc2 880.0
          }
          $> { asdr: makePiecewise pwf }
      )

piece :: Scene (SceneI Unit Unit) FFIAudio (Effect Unit) Frame0 Unit
piece =
  (const createFrame)
    @!> iloop \e { asdr } ->
        let
          { time, headroom } = e

          pulse = asdr { time, headroom: toNumber headroom / 1000.0 }

          g' = ff 0.04 $ head pulse

          fade
            | time < 1.0 = g' $> calcSlope 0.0 0.0 1.0 1.0 time
            | otherwise = g' $> 1.0

          g = g' * fade

          ramp = g $> calcSlopeExp 0.0 1.0 10.0 0.0 4.0 time
        in
          ichange
            { unit0: gain_ (g * ramp)
            , unit1: gain_ g
            , unit2: gain_ (g * (sub 1.0 <$> ramp))
            }
            $> { asdr: tail pulse }
