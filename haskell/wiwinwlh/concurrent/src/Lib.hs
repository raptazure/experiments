module Lib
  ( someFunc,
    example01,
    example02,
    example03,
    example04,
  )
where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    modifyTVar,
    newTVar,
    newTVarIO,
    readTVarIO,
  )
import Control.Monad (forever, replicateM_)
import Control.Parallel.Strategies (rpar, rseq, runEval)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Prelude hiding (take)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

example01 :: (t -> b) -> t -> t -> (b, b)
example01 f x y = runEval $ do
  a <- rpar $ f x
  b <- rpar $ f y
  rseq a
  rseq b
  return (a, b)

example02 :: IO Integer
example02 = do
  account1 <- newIORef 5000
  account2 <- newIORef 1000
  transfer 500 account1 account2
  readIORef account1

transfer :: Integer -> IORef Integer -> IORef Integer -> IO ()
transfer n from to = do
  modifyIORef from (+ (- n))
  modifyIORef to (+ n)

take :: MVar [Char] -> IO ()
take m = forever $ do
  x <- takeMVar m
  putStrLn x

put :: MVar [Char] -> IO ()
put m = do
  replicateM_ 5 $ do
    threadDelay 100000
    putMVar m "Value set."

example03 :: IO ()
example03 = do
  m <- newEmptyMVar
  forkIO (take m)
  put m

example04 :: IO Integer
example04 = do
  account1 <- newTVarIO 5000
  account2 <- newTVarIO 1000
  atomically (transfer' 500 account1 account2)
  readTVarIO account1

transfer' :: Num a => a -> TVar a -> TVar a -> STM ()
transfer' n from to = do
  modifyTVar from (+ (- n))
  modifyTVar to (+ n)
