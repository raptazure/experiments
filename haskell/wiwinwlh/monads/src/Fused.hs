{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Fused where

import Control.Algebra (Algebra, Effect, Has)
import Control.Carrier.Error.Either
  ( Throw,
    throwError,
  )
import Control.Carrier.Lift (runM)
import Control.Carrier.State.Lazy
  ( State,
    evalState,
    get,
    modify,
  )
import Control.Carrier.Throw.Either (runThrow)
import Control.Exception (ArithException (DivideByZero))
import Control.Monad.Identity (Identity)

-- m is called the carrier for the sig effect signature containing the eff effect
-- type Has eff sig m = (Members eff sig, Algebra sig m)

-- data State s m k = Get (s -> m k) | Put s (m k) deriving (Functor)
-- get :: Has (State s) sig m => m s
-- put :: Has (State s) sig m => s -> m ()

-- The Carrier for the State effect is defined as StateC and the evaluators for the state carrier are defined in the same interface as mtl except they evaluate into a result containing the effect parameter m
-- newtype StateC s m a = StateC (s -> m (s, a)) deriving (Functor)
-- runState :: s -> StateC s m a -> m (s, a)
-- runM :: LiftC m a -> m a
-- run :: Identity a -> a

example1 :: Has (State Integer) sig m => m Integer
example1 = do
  modify (+ 1)
  modify (* 10)
  get

ex1 :: (Algebra sig m, Effect sig) => m Integer
ex1 = evalState (1 :: Integer) example1

run1 :: Identity Integer
run1 = runM ex1

run2 :: IO Integer
run2 = runM ex1

-- composite effects

example2 :: (Has (State (Double, Double)) sig m, Has (Throw ArithException) sig m) => m Double
example2 = do
  (a, b) <- get
  if b == 0
    then throwError DivideByZero
    else pure (a / b)

ex2 :: (Algebra sig m, Effect sig) => m (Either ArithException Double)
ex2 = runThrow $ evalState (1 :: Double, 2 :: Double) example2

ex3 :: (Algebra sig m, Effect sig) => m (Either ArithException Double)
ex3 = evalState (1 :: Double, 0 :: Double) (runThrow example2)