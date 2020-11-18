module Lib
  ( someFunc,
  )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- tail recursion
total :: Num p => [p] -> p
total [] = 0
total (x : xs) = x + total xs

total' :: Num t => [t] -> t -> t
total' [] n = n
total' (x : xs) n = total' xs $! (n + x)

-- mutual recursion
even' :: (Eq a, Num a) => a -> Bool
even' 0 = True
even' n = odd' (n -1)

odd' :: (Eq a, Num a) => a -> Bool
odd' 0 = False
odd' n = even' (n -1)