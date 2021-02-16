module App.Button where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State
  = { count :: Int }

data Action
  = Increment

component :: forall t29 t30 t54 t57. H.Component HH.HTML t57 t54 t30 t29
component =
  H.mkComponent
    { initialState: \_ -> { count: 0 }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

maybeElem :: forall w i a. Maybe a -> (a -> HH.HTML w i) -> HH.HTML w i
maybeElem val f = case val of
  Just x -> f x
  _ -> HH.text ""

renderName :: forall w i. Maybe String -> HH.HTML w i
renderName mbName = maybeElem mbName \name -> HH.text name

whenElem :: forall w i. Boolean -> (Unit -> HH.HTML w i) -> HH.HTML w i
whenElem cond f = if cond then f unit else HH.text ""

-- Render the old number, but only if it is different from the new number
renderOld :: forall w i. { old :: Number, new :: Number } -> HH.HTML w i
renderOld { old, new } = 
  whenElem (old /= new) \_ -> 
    HH.div_ [ HH.text $ show old ]

render ::
  forall t12 t14 t3.
  Show t12 =>
  { count :: t12
  | t14
  } ->
  HH.HTML t3 Action
render state =
  HH.div_
    [ HH.p_
        [ HH.text $ "You clicked " <> show state.count <> " times" ]
    , HH.button
        [ HE.onClick \_ -> Just Increment ]
        [ HH.text "Click me" ]
    , HH.div_ (map HH.text [ "lake1", "lake2" ])
    ]

handleAction :: forall cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Increment -> H.modify_ \st -> st { count = st.count + 1 }
