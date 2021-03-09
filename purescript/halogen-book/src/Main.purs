module Main where

import Prelude
import App.HttpReq as HReq
import App.RandomNum as Random
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.HTML.HTMLDocument (body)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    -- runUI HReq.component unit body
    runUI Random.component unit body
