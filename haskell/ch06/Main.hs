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

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1 .. 10]))

-- zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]

addThree :: (Num a) => a -> a -> a -> a
addThree = \x -> \y -> \z -> x + y + z

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: Num a => [a] -> a
sum'' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

-- using foldr because of slower ++ operation
-- foldr is easier to deal with infinite lists

map''' :: (a -> b) -> [a] -> [b]
map''' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl1 (*)

head' :: [a] -> a
head' = foldl1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- scanr (+) 0 [3,5,2,1]
-- scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]
-- scanl (flip (:)) [] [3,2,1]

sqrtSums :: Int
sqrtSums = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1 ..]))) + 1

-- ($) :: (a -> b) -> a -> b
-- f $ x = f x

-- sum (map sqrt [1..130])
-- sum $ sqrt [1..130]
-- sum (filter (> 10) (map (*2) [2..10])
-- sum $ filter (> 10) $ map (*2) [2..10]
-- map ($ 3) [(4 +), (10 *), (^ 2), sqrt]

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f (g x)

-- map (negate . abs) [5,-3,-6,7,-3,2,-19,24]

-- map (\xs -> negate (sum (tail xs))) [[1 .. 5], [3 .. 6], [1 .. 7]]
-- map (negate . sum . tail) [[1 .. 5], [3 .. 6], [1 .. 7]]

-- sum (replicate 5 (max 6.7 8.9))
-- sum . replicate 5 . max 6.7 $ 8.9

-- replicate 100 (product (map (* 3) (zipWith max [1, 2, 3, 4, 5] [4, 5, 6, 7, 8])))
-- replicate 100 . product . map (* 3) . zipWith max [1, 2, 3, 4, 5] $ [4, 5, 6, 7, 8]

sum''' :: (Num a) => [a] -> a
sum''' = foldl (+) 0

fn = ceiling . negate . tan . cos . max 50

-- oddSquareSum :: Integer
-- oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

-- oddSquareSum :: Integer
-- oddSquareSum =
--     let oddSquares = filter odd $ map (^2) [1..]
--         belowLimit = takeWhile (<10000) oddSquares
--     in  sum belowLimit

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (< 10000) . filter odd . map (^ 2) $ [1 ..]