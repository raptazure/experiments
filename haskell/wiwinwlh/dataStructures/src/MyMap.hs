{- A map is an associative array mapping any instance of Ord keys to values of any type. -}

module MyMap
  ( kv,
    lkup,
  )
where

-- Initialization empty O(1)
-- Size size O(1)
-- Lookup lookup O(log(n))
-- Insertion insert O(log(n))
-- Traversal traverse O(n)

import qualified Data.Map as Map

kv :: Map.Map Integer String
kv = Map.fromList [(1, "a"), (2, "b")]

lkup :: Integer -> String -> String
lkup key def =
  case Map.lookup key kv of
    Just val -> val
    Nothing -> def