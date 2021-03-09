module App.HttpReq where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.Event.Event as Event

type State
  = { loading :: Boolean
    , username :: String
    , result :: Maybe String
    }

data Action
  = SetUsername String
  | MakeRequest Event

initialState :: forall i. i -> State
initialState _ = { loading: false, username: "", result: Nothing }

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.form
    [ HE.onSubmit \ev -> Just (MakeRequest ev) ]
    [ HH.h1_ [ HH.text "Look up GitHub user" ]
    , HH.label_
        [ HH.div_ [ HH.text "Enter username: " ]
        , HH.input
            [ HP.value st.username
            , HE.onValueInput \str -> Just (SetUsername str)
            ]
        ]
    , HH.button
        [ HP.disabled st.loading, HP.type_ HP.ButtonSubmit ]
        [ HH.text "Fetch Info" ]
    , HH.p_
        [ HH.text (if st.loading then "Working" else "") ]
    , HH.div_ case st.result of
        Nothing -> []
        Just res -> [ HH.h2_ [ HH.text "Response: " ], HH.pre_ [ HH.code_ [ HH.text res ] ] ]
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  SetUsername username -> do
    H.modify_ _ { username = username, result = Nothing }
  MakeRequest event -> do
    H.liftEffect $ Event.preventDefault event
    username <- H.gets _.username
    H.modify_ _ { loading = true }
    response <- H.liftAff $ AX.get AXRF.string ("https://api.github.com/users/" <> username)
    H.modify_ _ { loading = false, result = map _.body (hush response) }

component :: forall query input output m. MonadAff m => H.Component HH.HTML query input output m
component =
  H.mkComponent
    { initialState, render, eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
