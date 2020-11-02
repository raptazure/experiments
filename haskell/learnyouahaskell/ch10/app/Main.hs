module Main where

import Lib

main :: IO ()
main = do
  contents <- getContents
  let threes = groupsOf 3 (map read $ lines contents)
      roadSystem = map (\[a, b, c] -> Section a b c) threes
      path = optimalPath roadSystem
      pathString = concat $ map (show . fst) path
      pathPrice = sum $ map snd path
  putStrLn $ "The best path to take is: " ++ pathString
  putStrLn $ "The price is: " ++ show pathPrice

-- cat paths.txt | stack exec ch10-exe