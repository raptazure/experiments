module App.KeyBoard where

import Prelude
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.Event (eventListener)
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

type State
  = { chars :: String }

data Action
  = Initialize
  | HandleKey H.SubscriptionId KE.KeyboardEvent

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
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

initialState :: forall input. input -> State
initialState _ = { chars: "" }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.p_ [ HH.text "Hold down the shift key and type some characters!" ]
    , HH.p_ [ HH.text "Press ENTER or RETURN to clear and remove the event listener." ]
    , HH.p_ [ HH.text state.chars ]
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    document <- H.liftEffect $ document =<< window
    H.subscribe' \sid ->
      eventListener
        KET.keyup
        (HTMLDocument.toEventTarget document)
        (map (HandleKey sid) <<< KE.fromEvent)
  HandleKey sid ev
    | KE.shiftKey ev -> do
      H.liftEffect $ E.preventDefault $ KE.toEvent ev
      let
        char = KE.key ev
      when (String.length char == 1) do
        H.modify_ \st -> st { chars = st.chars <> char }
    | KE.key ev == "Enter" -> do
      H.liftEffect $ E.preventDefault (KE.toEvent ev)
      H.modify_ _ { chars = "" }
      H.unsubscribe sid
    | otherwise -> pure unit
