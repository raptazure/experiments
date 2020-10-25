multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- let multTwoWithNine = multThree 9
-- let multWithEighteen = multTwoWithNine 2
-- multWithEighteen 10

compareWithHundred :: (Num a, Ord a) => a -> Ordering
-- compareWithHundred x = compare 100 x
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- applyTwice (++ " HAHA") "HEY"
-- applyTwice (3:) [1]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

-- zipWith' (*) (replicate 5 2) [1..]

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

-- zipWith (flip' div) [2,2..] [10,8,6,4,2]

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

-- map' (+3) [1,5,3,1,6]
-- [x+3 | x <- [1,5,3,1,6]
-- map' (map' (^2)) [[1,2],[3,4,5,6],[7,8]]
-- map' fst [(1,2),(3,5),(6,3),(2,6),(2,5)]

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x : xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

-- let notNull x = not (null x) in filter' notNull [[1, 2, 3], [], [3, 4, 5], [2, 2], [], [], []]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort (filter (<= x) xs)
      biggerSorted = quicksort (filter (> x) xs)
   in smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999 ..])
  where
    p x = x `mod` 3829 == 0

-- sum (takeWhile (< 10000) (filter odd (map (^2) [1..])))
-- sum (takeWhile (< 10000) [m | m <- [n ^ 2 | n <- [1 ..]], odd m])

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1 .. 100]))
  where
    isLong xs = length xs > 15

-- let listOfFuns = map (*) [0..]
-- (listOfFuns !! 4) 5