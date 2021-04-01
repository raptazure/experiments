module Lib
  ( someFunc,
    example01,
    example02,
    example03,
    example04,
    example05,
    example06,
    actions,
    test01,
    test02,
    test03,
    timeit,
  )
where

import Control.Concurrent
  ( Chan,
    MVar,
    QSem,
    ThreadId,
    forkIO,
    myThreadId,
    newChan,
    newEmptyMVar,
    newQSem,
    putMVar,
    readChan,
    signalQSem,
    takeMVar,
    threadDelay,
    waitQSem,
    writeChan,
  )
import Control.Concurrent.Async (async, mapConcurrently, race, wait)
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    modifyTVar,
    newTVar,
    newTVarIO,
    readTVar,
    readTVarIO,
    retry,
  )
import Control.Monad (forM_, forever, replicateM_, when)
import Control.Parallel.Strategies (Eval, Strategy, rpar, rseq, runEval)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Time (diffUTCTime, getCurrentTime)
import Prelude hiding (take)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

example01 :: (t -> b) -> t -> t -> (b, b)
example01 f x y = runEval $ do
  a <- rpar $ f x
  b <- rpar $ f y
  rseq a
  rseq b
  pure (a, b)

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

type Account = TVar Double

actions :: Account -> Account -> [IO ThreadId]
actions a b =
  map
    forkIO
    [ atomically (transfer' 10 a b),
      atomically (transfer' (-20) a b),
      atomically (transfer' 30 a b)
    ]

example04 :: IO Double
example04 = do
  account1 <- newTVarIO 5000
  account2 <- newTVarIO 1000
  atomically (transfer' 500 account1 account2)
  readTVarIO account1

transfer' :: Double -> Account -> Account -> STM ()
transfer' n from to = do
  available <- readTVar from
  when (n > available) retry
  modifyTVar from (+ (- n))
  modifyTVar to (+ n)

producer :: Chan Integer -> IO ()
producer chan = forM_ [0 .. 1000] $ \i -> do
  writeChan chan i
  putStrLn "Writing to channel."

consumer :: Chan Integer -> IO ()
consumer chan = forever $ do
  val <- readChan chan
  thread <- myThreadId
  putStrLn ("Received item in thead: " ++ show thread)
  print val

example05 :: IO ()
example05 = do
  chan <- newChan
  forkIO (consumer chan)
  forkIO (consumer chan)
  forkIO (consumer chan)
  forkIO (consumer chan)
  pure ()

task :: Integer -> QSem -> IO ()
task index sem = do
  waitQSem sem
  forkIO $ putStrLn ("Thread: " ++ show index ++ "\n")
  signalQSem sem

example06 :: IO ()
example06 = do
  sem <- newQSem 1
  forkIO (task 1 sem)
  forkIO (task 2 sem)
  forkIO (task 3 sem)
  pure ()

parPair' :: Strategy (a, b)
parPair' (a, b) = do
  a' <- rpar a
  b' <- rpar b
  pure (a', b')

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

serial :: (Int, Int)
serial = (fib 30, fib 31)

parallel :: (Int, Int)
parallel = runEval . parPair' $ (fib 30, fib 31)

using :: a -> Strategy a -> a
x `using` s = runEval (s x)

parallel' :: (Int, Int)
parallel' = (fib 30, fib 31) `using` parPair'

parMap' :: (a -> b) -> [a] -> Eval [b]
parMap' f [] = pure []
parMap' f (a : as) = do
  b <- rpar (f a)
  bs <- parMap' f as
  pure (b : bs)

result :: [Int]
result = runEval $ parMap' (+ 1) [1 .. 1000]

timeit :: IO a -> IO (a, Double)
timeit io = do
  t0 <- getCurrentTime
  a <- io
  t1 <- getCurrentTime
  pure (a, realToFrac (t1 `diffUTCTime` t0))

worker :: Int -> IO Int
worker n = do
  threadDelay (10 ^ 2 * n)
  pure (n * n)

-- Spawn 2 threads in parallel, halt on both finished.
test01 :: IO (Int, Int)
test01 = do
  val1 <- async $ worker 1000
  val2 <- async $ worker 2000
  (,) <$> wait val1 <*> wait val2

-- Spawn 2 threads in parallel, halt on first finished.
test02 :: IO (Either Int Int)
test02 = do
  let val1 = worker 1000
  let val2 = worker 2000
  race val1 val2

-- Spawn 10 threads in parallel, halt on all finished.
test03 :: IO [Int]
test03 = mapConcurrently worker [0 .. 10]
