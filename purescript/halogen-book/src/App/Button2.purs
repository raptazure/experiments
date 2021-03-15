module App.Button2 where

import Prelude
import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Subscription as HS
import Type.Proxy (Proxy(..))

type Slots
  = ( button :: forall q. H.Slot q Void Unit )

_button = Proxy :: Proxy "button"

type ParentState
  = { count :: Int }

data ParentAction
  = Initialize
  | Increment

parent :: forall query input output m. MonadAff m => H.Component query input output m
parent =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              }
    }
  where
  initialState :: input -> ParentState
  initialState _ = { count: 0 }

  render :: ParentState -> H.ComponentHTML ParentAction Slots m
  render { count } = HH.div_ [ HH.slot_ _button unit button { label: show count } ]

  handleAction :: ParentAction -> H.HalogenM ParentState ParentAction Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      { emitter, listener } <- H.liftEffect HS.create
      void $ H.subscribe emitter
      void
        $ H.liftAff
        $ Aff.forkAff
        $ forever do
            Aff.delay $ Milliseconds 1000.0
            H.liftEffect $ HS.notify listener Increment
    Increment -> H.modify_ \st -> st { count = st.count + 1 }

-- Now we turn to our child component, the button.
data ButtonAction
  = Receive ButtonInput

type ButtonInput
  = { label :: String }

type ButtonState
  = { label :: String }

button :: forall query output m. H.Component query ButtonInput output m
button =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , receive = Just <<< Receive
              }
    }
  where
  initialState :: ButtonInput -> ButtonState
  initialState { label } = { label }

  render :: ButtonState -> H.ComponentHTML ButtonAction () m
  render { label } = HH.button_ [ HH.text label ]

  handleAction :: ButtonAction -> H.HalogenM ButtonState ButtonAction () output m Unit
  handleAction = case _ of
    -- When we receive new input we update our `label` field in state.
    Receive input -> H.modify_ _ { label = input.label }
