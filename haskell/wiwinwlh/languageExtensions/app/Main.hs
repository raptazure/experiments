module Main where

import Lib (headNil)

main :: IO ()
main = do
  print $ headNil [1, 2, 3]
