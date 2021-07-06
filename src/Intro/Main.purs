module Intro.Main where

import Prelude

import Control.Comonad.Cofree (Cofree, mkCofree)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import FRP.Event (makeEvent, subscribe)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS


type State
  = { myText :: String
    }

data Action
  = Initialize | UpdateUI String

component :: forall query input output m. MonadEffect m => MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }

initialState :: forall input. input -> State
initialState _ =
  { myText: mempty
  }

classes :: forall r p. Array String -> HP.IProp ( class :: String | r ) p
classes = HP.classes <<< map ClassName

data FizzBang = Fizz | Bang

render :: forall m. State -> H.ComponentHTML Action () m
render { myText } =
  HH.div [ classes [ "w-screen", "h-screen" ] ]
    [ HH.div [ classes [ "flex", "flex-col", "w-full", "h-full" ] ]
        [ HH.div [ classes [ "flex-grow" ] ] []
        , HH.div [ classes [ "flex-grow-0", "flex", "flex-row" ] ]
            [ HH.div [ classes [ "flex-grow" ] ] []
            , HH.div_
                [ HH.h1 [ classes [ "text-center", "text-3xl", "font-bold" ] ]
                    [ HH.text "Intro to FRP" ]
                ]
            , HH.div [ classes [ "flex-grow" ] ] []
            ]
        , HH.div [ classes [ "flex-grow-0", "flex", "flex-row" ] ]
            [ HH.div [ classes [ "flex-grow" ] ] []
            , HH.div_
                [ HH.text myText
                ]
            , HH.div [ classes [ "flex-grow" ] ] []
            ]
        , HH.div [ classes [ "flex-grow" ] ] []
        ]
    ]


handleAction :: forall output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    { emitter, listener } <- H.liftEffect HS.create
    let delFac = 300.0
    let myEvent = makeEvent \f -> do
          launchAff_ do
            liftEffect $ f "hello"
            delay (Milliseconds delFac)
            liftEffect $ f "world"
            delay (Milliseconds delFac)
            liftEffect $ f "hello"
            delay (Milliseconds delFac)
            liftEffect $ f "world"
            delay (Milliseconds delFac)
            liftEffect $ f "hello"
            delay (Milliseconds delFac)
            liftEffect $ f "world"
            delay (Milliseconds delFac)
            liftEffect $ f "hello"
            delay (Milliseconds delFac)
            liftEffect $ f "world"
            delay (Milliseconds delFac)
            liftEffect $ f "hello"
            delay (Milliseconds delFac)
            liftEffect $ f "world"
            delay (Milliseconds delFac)
            liftEffect $ f "hello"
            delay (Milliseconds delFac)
            liftEffect $ f "world"
            delay (Milliseconds delFac)
          pure $ pure unit
    _ <- H.liftEffect $ subscribe myEvent \str -> HS.notify listener (UpdateUI str)
    let a x n = mkCofree n (Identity (a (if true then Fizz else Bang) (n + 1))) 
    void $ H.subscribe emitter
    H.liftEffect $ HS.notify listener (UpdateUI "Goals: Learn Events, Learn Behaviors, Learn Streams, Learn Linear Types")
  UpdateUI myText -> do
    H.modify_ _ { myText = myText }
