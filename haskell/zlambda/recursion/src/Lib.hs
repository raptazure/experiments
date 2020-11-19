module Lib
  ( someFunc,
  )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- tail recursion
total :: Num p => [p] -> p
total [] = 0
total (x : xs) = x + total xs

total' :: Num t => [t] -> t -> t
total' [] n = n
total' (x : xs) n = total' xs $! (n + x)

-- mutual recursion
even' :: (Eq a, Num a) => a -> Bool
even' 0 = True
even' n = odd' (n - 1)

odd' :: (Eq a, Num a) => a -> Bool
odd' 0 = False
odd' n = even' (n - 1)

-- fibonacci
fibStep :: Num a => (a, a) -> (a, a)
fibStep (u, v) = (v, u + v)

fibPair :: (Eq t, Num t, Num b) => t -> (b, b)
fibPair 0 = (0, 1)
fibPair n = fibStep (fibPair (n - 1))

fastFib :: Integer -> Integer
fastFib = fst . fibPair

fibs :: Integer -> [Integer]
fibs n = map fastFib [1 .. n]

fibs' :: Num a => Int -> [a]
fibs' n = take n (map fst (iterate fibStep (0, 1)))

fib :: (Eq t1, Num t1, Num t2) => t1 -> t2 -> t2 -> t2
fib 0 x1 x2 = x2
fib n x1 x2 = fib (n - 1) x2 (x1 + x2)

fibnacci :: (Ord t1, Num t1, Num p) => t1 -> p
fibnacci n
  | n < 2 = 1
  | otherwise = fib (n - 2) 1 1

golden :: Fractional a => Int -> [a]
golden n = take n (map (\(x, y) -> x / y) (iterate fibStep (0, 1)))

combine :: [(a, a)] -> [(a, a, a)]
combine ((x1, x2) : (x3, x4) : xs) = (x1, x2, x4) : combine ((x3, x4) : xs)
combine _ = []

fibPairs :: Int -> [(Int, Int)]
fibPairs n = map fibPair [1 .. n]

difference :: Int -> [Int]
difference n = map (\(x1, x2, x3) -> x1 * x3 - x2 * x2) (combine $ fibPairs n)

-- romeNotation
romeNotation :: [String]
romeNotation = ["M", "CM", "D", "CD", "C", "XC", "XL", "L", "X", "IX", "V", "IV", "I"]

romeAccount :: [Int]
romeAccount = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]

pair :: [(Int, String)]
pair = zip romeAccount romeNotation

subtrahend :: Int -> (Int, String)
subtrahend n = head (dropWhile (\(x, _) -> x > n) pair)

convert :: Int -> (Int, String)
convert 0 = (0, "")
convert n =
  let (i, st) = subtrahend n
   in let (i', st') = convert (n - i) in (i', st ++ st')

-- binary search
search :: (Ord a) => a -> [a] -> Bool
search x [] = False
search x xs
  | m < x = search x behind
  | m > x = search x front
  | otherwise = True
  where
    (front, m : behind) = splitAt (length xs `div` 2) xs

-- hanoi
move :: (Eq a1, Num a1) => (a1, a2, a2, a2) -> [(a2, a2)]
move (1, from, to, via) = [(from, to)]
move (n, from, to, via) = move (n -1, from, via, to) ++ [(from, to)] ++ move (n -1, via, to, from)

hanoi :: (Eq a1, Num a1, Num a2) => a1 -> [(a2, a2)]
hanoi n = move (n, 1, 2, 3)