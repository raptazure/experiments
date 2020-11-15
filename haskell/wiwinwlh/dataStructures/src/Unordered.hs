module Unordered
  ( example1,
    example2,
  )
where

-- Initialization empty O(1)
-- Size size O(1)
-- Insertion insert O(log(n))
-- Deletion delete O(log(n))
-- Traversal traverse O(n)

-- fromList :: (Eq k, Hashable k) => [(k, v)] -> HashMap k v
-- lookup :: (Eq k, Hashable k) => k -> HashMap k v -> Maybe v
-- insert :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v

import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S

example1 :: M.HashMap Int Char
example1 = M.fromList $ zip [1 .. 10] ['a' ..]

example2 :: S.HashSet Int
example2 = S.fromList [1 .. 10]
