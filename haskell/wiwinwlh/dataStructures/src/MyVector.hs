{- Vectors are high performance single dimensional arrays that comes in six variants, two for each of the following types of a mutable and an immutable variant. -}

module MyVector
  ( norm,
    example1,
    vec,
    vecIO,
    vecST,
  )
where

-- Initialization empty O(1)
-- Size length O(1)
-- Indexing (!) O(1)
-- Append append O(n)
-- Traversal traverse O(n)

-- fromList :: [a] -> Vector a
-- toList :: Vector a -> [a]
-- (!) :: Vector a -> Int -> a
-- map :: (a -> b) -> Vector a -> Vector b
-- foldl :: (a -> b -> a) -> a -> Vector b -> a
-- scanl :: (a -> b -> a) -> a -> Vector b -> Vector a
-- zipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
-- iterateN :: Int -> (a -> a) -> a -> Vector a

import Control.Monad as M
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Vector.Unboxed (freeze)
import Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable
import GHC.Prim

norm :: Vector Double -> Double
norm = sqrt . V.sum . V.map (\x -> x * x)

vec :: Vector Double
vec = V.fromList [10.3, 20.5, 30.6, 40]

example1 :: Double
example1 = norm $ V.iterateN 1000 (+ 1) 0.0

{- Mutable vectors are variants of vectors which allow inplace updates. -}

-- Initialization empty O(1)
-- Size length O(1)
-- Indexing (!) O(1)
-- Append append O(n)
-- Traversal traverse O(n)
-- Update modify O(1)
-- Read read O(1)
-- Write write O(1)

-- freeze :: MVector (PrimState m) a -> m (Vector a)
-- thaw :: Vector a -> MVector (PrimState m) a

example :: PrimMonad m => m (V.Vector Int)
example = do
  v <- new 10
  M.forM_ [0 .. 9] $ \i ->
    write v i (2 * i)
  freeze v

-- vector computation in IO
vecIO :: IO (V.Vector Int)
vecIO = example

-- vector computation in ST
vecST :: ST s (V.Vector Int)
vecST = example