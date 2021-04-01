module Main where

import Control.Concurrent.STM (atomically, newTVarIO, readTVar, readTVarIO)
import Lib
  ( actions,
    example01,
    example02,
    example03,
    example04,
    example05,
    example06,
    someFunc,
    test01,
    test02,
    test03,
    timeit,
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
  accountA <- newTVarIO 60
  accountB <- newTVarIO 0
  sequence_ (actions accountA accountB)
  balanceA <- readTVarIO accountA
  balanceB <- readTVarIO accountB
  print balanceA
  print balanceB
  print =<< timeit test01
  print =<< timeit test02
  print =<< timeit test03
