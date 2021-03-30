module Main where

import Lib
  ( example01,
    example02,
    example03,
    example04,
    example05,
    example06,
    someFunc,
  )
import System.IO
  ( BufferMode (LineBuffering),
    hSetBuffering,
    stdout,
  )

main :: IO ()
main = do
  someFunc
  a <- example02
  b <- example04
  print a
  print b
  example03
  hSetBuffering stdout LineBuffering
  example05
  example06
