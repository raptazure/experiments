module ProblemOne
  ( answer,
  )
where

sumSeries :: Int -> Int
sumSeries n = n * (n + 1) `div` 2

sumMultiplesBelow :: Int -> Int -> Int
sumMultiplesBelow factor cap =
  let n = ceiling (fromIntegral cap / fromIntegral factor) - 1
   in factor * sumSeries n

answer :: IO ()
answer =
  let threes = sumMultiplesBelow 3 1000
      fives = sumMultiplesBelow 5 1000
      fifteens = sumMultiplesBelow 15 1000
   in print $ threes + fives - fifteens