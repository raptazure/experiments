module Listgen where

-- Eight Queen
-- 8! = 403320
insert :: a -> [a] -> [[a]]
insert n [] = [[n]]
insert n (n' : ns) = (n : n' : ns) : [n' : ns' | ns' <- insert n ns]

permutation :: [a] -> [[a]]
permutation [] = [[]]
permutation (x : xs) = concat [insert x permux | permux <- permutation xs]

noSameDiag :: (Eq a, Num a, Enum a) => [a] -> Bool
noSameDiag [] = True
noSameDiag xs@(x : xs') =
  and [abs (i1 - i) /= abs (p1 - p) | (i, p) <- ip] && noSameDiag xs'
  where
    (i1, p1) : ip = zip [1 ..] xs

queen :: (Num a, Enum a, Eq a) => a -> [[a]]
queen n = [xs | xs <- permutation [1 .. n], noSameDiag xs]
