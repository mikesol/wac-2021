module Audio where

import Prelude

import Control.Apply.Indexed ((:*>))
import Control.Comonad.Cofree (Cofree, deferCofree, head, tail)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.Lens (_1, over, traversed)
import Data.List (List(..), (..), (:))
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList(..))
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (D3, D4, D5)
import Data.Vec ((+>))
import Data.Vec as V
import Math (pow)
import Record as R
import WAGS.Change (ichange)
import WAGS.Control.Functions.Validated (iloop, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Graph.AudioUnit (OnOff(..), TGain, TPeriodicOsc, TSpeaker)
import WAGS.Graph.Parameter (ff)
import WAGS.Interpret (class AudioInterpret)
import WAGS.Math (calcSlope)
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

osc3 :: POsc D3
osc3 = (0.0 +> 0.01 +> -0.2 +> V.empty) /\ (0.0 +> 0.01 +> 0.02 +> V.empty)

type SceneType
  = { speaker :: TSpeaker /\ { mix :: Unit }
    , mix :: TGain /\ { unit0 :: Unit, unit1 :: Unit, unit2 :: Unit, unit3 :: Unit }
    , unit0 :: TGain /\ { osc0 :: Unit }
    , osc0 :: TPeriodicOsc /\ {}
    , unit1 :: TGain /\ { osc1 :: Unit }
    , osc1 :: TPeriodicOsc /\ {}
    , unit2 :: TGain /\ { osc2 :: Unit }
    , osc2 :: TPeriodicOsc /\ {}
    , unit3 :: TGain /\ { osc3 :: Unit }
    , osc3 :: TPeriodicOsc /\ {}
    }

type FrameTp a e p i o x
  = IxWAG a e p Unit i o x

envE = 0.11 :: Number

pwf' :: List (Number /\ Number)
pwf' = (0.03 /\ 1.0) : (0.07 /\ 0.1) : (0.09 /\ 0.0) : (envE /\ 0.0) : Nil

pwf :: NonEmpty List (Number /\ Number)
pwf =
  (0.00 /\ 0.0)
    :| (join $ map (\i -> over (traversed <<< _1) (add (envE * toNumber i)) pwf') (0 .. 200))

endT :: Number
endT = fst $ NEL.last $ NonEmptyList pwf

type CIN
  = Cofree Identity Number

ne2cf :: forall a. NonEmpty List a -> Cofree Identity a
ne2cf i = go i i
  where
  go :: NonEmpty List a -> NonEmpty List a -> Cofree Identity a
  go (x :| y) (a :| Nil) = go (x :| y) (a :| (x : y))

  go x (a :| (b : c)) = deferCofree \_ -> a /\ (Identity $ go x (b :| c))

type Control
  = { asdr :: ASDR, osc0 :: CIN, osc1 :: CIN, osc2 :: CIN, osc3 :: CIN }

cofrees =
  { osc0: ne2cf (66.0 :| 62.0 : 59.0 : 55.0 : 58.0 : 59.0 : 57.0 : Nil)
  , osc1: ne2cf (63.0 :| 54.0 : 54.0 : 50.0 : 55.0 : 52.0 : 54.0 : Nil)
  , osc2: ne2cf (58.0 :| 48.0 : 50.0 : 46.0 : 50.0 : 43.0 : 48.0 : Nil)
  , osc3: ne2cf (47.0 :| 45.0 : 43.0 : 41.0 : 39.0 : 33.0 : 38.0 : Nil)
  } ::
    { osc0 :: Cofree Identity Number
    , osc1 :: Cofree Identity Number
    , osc2 :: Cofree Identity Number
    , osc3 :: Cofree Identity Number
    }

createFrame ::
  forall audio engine. AudioInterpret audio engine =>
  FrameTp audio engine Frame0 {} SceneType Control
createFrame =
  ipatch
    :*> ( ichange
          { mix: 1.0
          , unit0: 0.0
          , unit1: 0.0
          , unit2: 0.0
          , unit3: 0.0
          , osc0: { waveform: osc0, onOff: On, freq: midi2cps 66.0 }
          , osc1: { waveform: osc1, onOff: On, freq: midi2cps 63.0 }
          , osc2: { waveform: osc2, onOff: On, freq: midi2cps 58.0 }
          , osc3: { waveform: osc3, onOff: On, freq: midi2cps 47.0 }
          }
          $> R.union { asdr: makePiecewise pwf }
              { osc0: unwrap $ tail cofrees.osc0
              , osc1: unwrap $ tail cofrees.osc1
              , osc2: unwrap $ tail cofrees.osc2
              , osc3: unwrap $ tail cofrees.osc3
              }
      )

data Events
  = StartExample
  | MouseDown

derive instance eqEvents :: Eq Events

midi2cps :: Number -> Number
midi2cps i = 440.0 * (2.0 `pow` ((i - 69.0) / 12.0))

piece :: forall audio engine. AudioInterpret audio engine => Scene (SceneI Events Unit) audio engine Frame0 Unit
piece =
  (const createFrame)
    @!> iloop \e ctrl ->
        let
          { time, headroom, active, trigger } = e

          pulse = ctrl.asdr { time, headroom: toNumber headroom / 1000.0 }

          g' = ff 0.04 $ head pulse

          fade
            | time < 1.0 = g' $> calcSlope 0.0 0.0 1.0 1.0 time
            | otherwise = g' $> 1.0

          g = g' * fade

          ramp = g $> calcSlope 0.0 1.0 endT 0.0 time

          ch =
            { unit0: g
            , unit1: g * ramp
            , unit2: g * (sub 1.0 <$> ramp)
            , unit3: g
            }
        in
          if active && trigger == MouseDown then
            ichange
              ( R.union
                  { osc0: midi2cps $ head ctrl.osc0
                  , osc1: midi2cps $ head ctrl.osc1
                  , osc2: midi2cps $ head ctrl.osc2
                  , osc3: midi2cps $ head ctrl.osc3
                  }
                  ch
              )
              $> ctrl
                  { asdr = tail pulse
                  , osc0 = unwrap $ tail ctrl.osc0
                  , osc1 = unwrap $ tail ctrl.osc1
                  , osc2 = unwrap $ tail ctrl.osc2
                  , osc3 = unwrap $ tail ctrl.osc3
                  }
          else
            ichange ch
              $> ctrl { asdr = tail pulse }
