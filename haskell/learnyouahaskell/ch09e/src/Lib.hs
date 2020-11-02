module Lib
  ( someFunc,
  )
where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import System.Random

someFunc :: IO ()
someFunc = putStrLn "someFunc"

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, newGen'') = random newGen'
   in (firstCoin, secondCoin, thirdCoin)

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value : randoms' newGen

finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
  let (value, newGen) = random gen
      (restOfList, finalGen) = finiteRandoms (n -1) newGen
   in (value : restOfList, finalGen)

-- randomR :: RandomGen g => (a, a) -> g -> (a, g)

l :: (Int, StdGen)
l = randomR (1, 6) (mkStdGen 35273)

randomStr = take 10 $ randomRs ('a', 'z') (mkStdGen 3) :: [Char]

p = B.fromChunks [S.pack [40, 41, 42], S.pack [43, 44, 45], S.pack [46, 47, 48]]

-- strict version: cons' - insert a lot of bytes
c = foldr B.cons' B.empty [50 .. 60]
