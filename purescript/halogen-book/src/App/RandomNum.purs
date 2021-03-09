module App.RandomNum where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Random (random)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    runUI component unit body

type State
  = Maybe Number

data Action
  = Regenerate

component :: forall query input output m. MonadEffect m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
initialState _ = Nothing

render :: forall m. State -> H.ComponentHTML Action () m
render state = do
  let
    value = maybe "No number generated yet" show state
  HH.div_
    [ HH.h1_
        [ HH.text "Random number" ]
    , HH.p_
        [ HH.text ("Current value: " <> value) ]
    , HH.button
        [ HE.onClick \_ -> Regenerate ]
        [ HH.text "Generate new number" ]
    ]

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Regenerate -> do
    newNumber <- H.liftEffect random
    H.modify_ \_ -> Just newNumber
