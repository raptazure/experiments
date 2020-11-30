module Lib
  ( someFunc,
  )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

