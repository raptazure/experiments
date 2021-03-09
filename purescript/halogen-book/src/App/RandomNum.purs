module App.RandomNum where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Effect.Class (class MonadEffect)
import Effect.Random (random)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State
  = Maybe Number

data Action
  = Regenerate

initialState :: forall input. input -> State
initialState _ = Nothing

render :: forall m. State -> H.ComponentHTML Action () m
render st = do
  let
    value = maybe "No number generated yet" show st
  HH.div_
    [ HH.h1_ [ HH.text "Random number" ]
    , HH.p_ [ HH.text ("Current value: " <> value) ]
    , HH.button
        [ HE.onClick \_ -> Just Regenerate ]
        [ HH.text "Generate new number" ]
    ]

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Regenerate -> do
    newNum <- H.liftEffect random
    H.modify_ \_ -> Just newNum

component :: forall query input output m. MonadEffect m => H.Component HH.HTML query input output m
component =
  H.mkComponent
    { initialState, render, eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
