{- Sets are unordered data structures containing Ord values of any type and guaranteeing uniqueness with in the structure. -}

module MySet
  ( set,
    memtest,
  )
where

-- Initialization empty O(1)
-- Size size O(1)
-- Insertion insert O(log(n))
-- Deletion delete O(log(n))
-- Traversal traverse O(n)
-- Membership Test member O(log(n))

import qualified Data.Set as Set

set :: Set.Set Integer
set = Set.fromList [1 .. 1000]

memtest :: Integer -> Bool
memtest elt = Set.member elt set