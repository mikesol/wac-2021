module Example3.Audio where

import Prelude

import Control.Applicative.Indexed ((:*>))
import Control.Comonad.Cofree (Cofree, deferCofree, head, tail)
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.NonEmpty ((:|), NonEmpty)
import Data.Profunctor (lcmap)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Vec as V
import Heterogeneous.Mapping (hmap)
import Math (pi, pow, sin, (%))
import Mezgeb as M
import Prim.Row (class Lacks)
import Record as R
import WAGS.Change (ichange)
import WAGS.Control.Functions (icont)
import WAGS.Control.Functions.Validated (ibranch, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene, WAG)
import WAGS.Graph.AudioUnit (OnOff(..), TBandpass, TGain, TLoopBuf, TSawtoothOsc, TSpeaker)
import WAGS.Interpret (class AudioInterpret)
import WAGS.Lib.SFofT (SFofT', nonEmptyToSFofT')
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

spacedNE2CF :: forall a. Number -> NonEmpty List a -> SFofT' a
spacedNE2CF space l =
  nonEmptyToSFofT' Nothing
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

type SteadyAcc r
  = ( funds :: CIN, freqA :: CIN, tz :: CIN, rt :: CIN | r )

type Step1Acc' o
  = { bpf0 :: o
    , bpf1 :: o
    , bpf2 :: o
    , bpf3 :: o
    , bpf4 :: o
    | SteadyAcc ()
    }

type Step1Acc
  = Step1Acc' (SFofT' Number)

withSteady :: forall r x. Lacks "freqA" x => Lacks "funds" x => Lacks "rt" x => Lacks "tz" x => { | x } -> { | SteadyAcc r } -> { | SteadyAcc x }
withSteady x sa = M.insert (M.get { funds: u, freqA: u, tz: u, rt: u } sa) x

initialStep1Acc :: forall r. { | SteadyAcc r } -> Step1Acc
initialStep1Acc =
  withSteady
    { bpf0: spacedNE2CF 0.04 (1.0 :| 1.5 : 2.0 : 1.5 : 1.0 : 0.5 : 1.0 : Nil)
    , bpf1: spacedNE2CF 0.06 (3.0 :| 4.0 : 5.0 : Nil)
    , bpf2: spacedNE2CF 0.03 (5.0 :| 5.1 : 5.2 : 5.3 : 7.0 : Nil)
    , bpf3: spacedNE2CF 0.05 (8.0 :| 9.0 : Nil)
    , bpf4: spacedNE2CF 0.07 (10.0 :| 10.2 : 10.6 : 10.3 : 11.8 : 13.1 : 12.5 : 16.4 : 9.3 : 14.6 : Nil)
    }

type Step2Acc' o
  = { osc :: o | SteadyAcc () }

type Step2Acc
  = Step2Acc' (SFofT' Number)

initialStep2Acc :: forall r. { | SteadyAcc r } -> Step2Acc
initialStep2Acc sa =
  M.insert (M.get { funds: u, freqA: u, tz: u, rt: u } sa)
    { osc:
        spacedNE2CF 0.2
          $ map (flip sub 12.0)
              ( 42.0 :| 47.0
                  : 43.0
                  : 46.0
                  : 51.0
                  : 140.0
                  : 44.0
                  : 45.0
                  : Nil
              )
    }

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

ne2cf :: forall a. NonEmpty List a -> Cofree Identity a
ne2cf i = go i i
  where
  go :: NonEmpty List a -> NonEmpty List a -> Cofree Identity a
  go (x :| y) (a :| Nil) = go (x :| y) (a :| (x : y))

  go x (a :| (b : c)) = deferCofree \_ -> a /\ (Identity $ go x (b :| c))

ne2cfi :: forall a. Semiring a => NonEmpty List a -> Cofree Identity a
ne2cfi i = go zero i i
  where
  go :: a -> NonEmpty List a -> NonEmpty List a -> Cofree Identity a
  go ig (x :| y) (a :| Nil) = go ig (x :| y) (a :| (x : y))

  go ig x (a :| (b : c)) = let acc = a + ig in deferCofree \_ -> acc /\ (Identity $ go acc x (b :| c))

type CIN
  = Cofree Identity Number

steady =
  { funds: ne2cf (123.0 :| 100.0 : 180.0 : 96.0 : 120.0 : 223.0 : 313.0 : 67.0 : Nil)
  , freqA: ne2cf (0.0 :| 5.0 : -7.0 : 12.0 : -14.0 : Nil)
  , rt: ne2cf (1.0 :| 1.13 : 0.95 : 1.03 : 0.3 : 1.02 : Nil)
  , tz: ne2cfi (5.8 :| 7.0 : 3.7 : 2.0 : 0.3 : 1.8 : 0.1 : 0.9 : 0.4 : 2.1 : Nil)
  } ::
    { | SteadyAcc () }

createFrame ::
  forall audio engine.
  AudioInterpret audio engine =>
  FrameTp audio engine Frame0 {} BaseSceneClosed Step1Acc
createFrame =
  ipatch
    :*> ichange
        { mix: 0.1
        , pad: 1.0
        , osc: { freq: head $ steady.funds, onOff: On }
        , bpf0: { q: 40.0 }
        , bpf1: { q: 40.0 }
        , bpf2: { q: 40.0 }
        , bpf3: { q: 40.0 }
        , bpf4: { q: 40.0 }
        }
    $> initialStep1Acc
        { funds: unwrap $ tail steady.funds
        , freqA: unwrap $ tail steady.freqA
        , rt: unwrap $ tail steady.rt
        , tz: steady.tz
        }

type CFAp
  = (Number -> (Cofree ((->) Number) Number)) -> (Cofree ((->) Number) Number)

type CFTail
  = Cofree ((->) Number) Number -> Number -> Cofree ((->) Number) Number

type CFHead
  = Cofree ((->) Number) Number -> Number

u = unit :: Unit

midi2cps :: Number -> Number
midi2cps i = 440.0 * (2.0 `pow` ((i - 69.0) / 12.0))

step2 ::
  forall audio engine proof.
  AudioInterpret audio engine =>
  ContTp audio engine proof BaseSceneOpen Step2Acc
step2 =
  ibranch \e a ->
    let
      loopStart = 1.0 + 1.0 * sin (3.0 * e.time * pi)

      actualized = hmap (((#) e.time) :: CFAp) (M.get { osc: u } a)

      heads = hmap ((midi2cps <<< add (head a.freqA) <<< head) :: CFHead) actualized
    in
      if e.time < head a.tz then
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
          $> M.insert (hmap (tail :: CFTail) actualized)
              (M.get { funds: u, freqA: u, tz: u, rt: u } a)
      else
        Left
          $ icont step1
              ( ipatch
                  :*> ichange
                      { bpf0: { q: 40.0 }
                      , bpf1: { q: 40.0 }
                      , bpf2: { q: 40.0 }
                      , bpf3: { q: 40.0 }
                      , bpf4: { q: 40.0 }
                      , osc: head a.funds
                      }
                  $> initialStep1Acc
                      { funds: unwrap $ tail a.funds
                      , freqA: a.freqA
                      , rt: a.rt
                      , tz: unwrap $ tail a.tz
                      }
              )

halfway = 5.8 :: Number

fullway = 2.0 * halfway :: Number

step1 ::
  forall audio engine proof.
  AudioInterpret audio engine =>
  ContTp audio engine proof BaseSceneClosed Step1Acc
step1 =
  ibranch \e a ->
    let
      actualized = hmap (((#) e.time) :: CFAp) (M.get { bpf0: u, bpf1: u, bpf2: u, bpf3: u, bpf4: u } a)

      heads = hmap (((_ * head a.funds) <<< head) :: CFHead) actualized
    in
      if e.time < head a.tz then
        Right $ ichange heads
          $> M.insert (hmap (tail :: CFTail) actualized)
              (M.get { funds: u, freqA: u, tz: u, rt: u } a)
      else
        Left
          $ icont step2
              ( ipatch
                  :*> ichange
                      { bpf0: { freq: 1000.0, q: 13.0 }
                      , bpf1: { freq: 1400.0, q: 13.0 }
                      , bpf2: { freq: 2000.0, q: 13.0 }
                      , bpf3: { freq: 2400.0, q: 13.0 }
                      , bpf4: { freq: 3000.0, q: 13.0 }
                      , buf: { buffer: "chaos", playbackRate: head a.rt, loopStart: 2.0, loopEnd: 2.2, onOff: On }
                      }
                  $> initialStep2Acc
                      { funds: a.funds
                      , freqA: unwrap $ tail a.freqA
                      , rt: unwrap $ tail a.rt
                      , tz: unwrap $ tail a.tz
                      }
              )

type PieceRepl
  = Scene (SceneI Unit Unit) Unit Instruction Frame0 Unit

pieceRepl :: SceneI Unit Unit
pieceRepl =
  { trigger: Nothing
  , world: unit
  , time: 0.0
  , sysTime: Milliseconds 0.0
  , headroom: 10
  }

piece ::
  forall audio engine.
  AudioInterpret audio engine =>
  SceneTp audio engine Frame0
piece = const createFrame @!> step1
