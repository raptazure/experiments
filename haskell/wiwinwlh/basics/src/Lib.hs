module Lib
  ( someFunc,
  )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Currying

add :: Int -> Int -> Int
add x y = x + y

-- curry :: ((a, b) -> c) -> a -> b -> c
-- uncurry :: (a -> b -> c) -> (a, b) -> cdd

uncurryAdd :: (Int, Int) -> Int
uncurryAdd = uncurry add

example :: Int
example = uncurryAdd (1, 2)