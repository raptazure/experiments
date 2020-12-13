{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Poly where

-- use a free monad as an intermediate form

import Polysemy
import Polysemy.Error
import Polysemy.State
import Polysemy.Trace

-- Members [ .. effects .. ] => Sem r a
-- Members '[Trace, State Example, Error MyError] r => Sem r ()
-- runError :: Sem (Error e ': r) a -> Sem e (Either e a)
-- runState :: s -> Sem (State s ': r) a -> Sem r (s, a)
-- runTraceList :: Sem (Trace ': r) a -> Sem r ([String], a)
-- runFinal :: Monad m => Sem '[Final m] a -> m a
-- embedToFinal :: (Member (Final m) r, Functor m) => Sem (Embed m ': r) a -> Sem r a

-- a simple stateful computation with only a single effect

data Example = Example {x :: Int, y :: Int} deriving (Show)

example1 :: Member (State Example) r => Sem r ()
example1 = do
  modify $ \s -> s {x = 1}
  pure ()

runExample1 :: IO ()
runExample1 = do
  (result, _) <-
    runFinal $
      embedToFinal @IO $
        runState (Example 0 0) example1
  print result

-- combines multiple effects

data MyError = MyError deriving (Show)

-- stateful update to Example data structure, with errors and tracing

example2 :: Members '[Trace, State Example, Error MyError] r => Sem r ()
example2 = do
  modify $ \s -> s {x = 1, y = 2}
  trace "foo"
  throw MyError
  pure ()

runExample2 :: IO ()
runExample2 = do
  result <-
    runFinal
      $embedToFinal
      @IO
      $errorToIOFinal
      @MyError
      $runState
      (Example 0 0)
      $traceToIO
      example2
  print result

-- GHC flags:  -flate-specialise, -fspecialise-aggressively