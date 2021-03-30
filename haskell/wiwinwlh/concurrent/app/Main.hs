module Main where

import Lib (example01, example02, example03, example04, someFunc)

main :: IO ()
main = do
  someFunc
  a <- example02
  b <- example04
  print a
  print b
  example03
