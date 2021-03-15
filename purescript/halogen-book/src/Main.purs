module Main where

import Prelude
import App.HttpReq as HReq
import App.KeyBoard as KeyBoard
import App.RandomNum as Random
import App.Timer as Timer
import App.Button2 as Button2
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    -- runUI HReq.component unit body
    -- runUI Random.component unit body
    -- runUI KeyBoard.component unit body
    -- runUI Timer.component unit body
    runUI Button2.parent unit body
