module Lib
  ( someFunc,
  )
where

import Data.Function (on)
import Data.List (sort, sortBy)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{- Currying -}

add :: Int -> Int -> Int
add x y = x + y

-- curry :: ((a, b) -> c) -> a -> b -> c
-- uncurry :: (a -> b -> c) -> (a, b) -> cdd

uncurryAdd :: (Int, Int) -> Int
uncurryAdd = uncurry add

example :: Int
example = uncurryAdd (1, 2)

{- Algebraic Datatype -}

data List' a = Nil' | List' a (List' a)

data List a = Nil | a :+: (List a)

res1 = take 15 powersOfTwo where powersOfTwo = iterate (2 *) 1

{- Pattern Matching -}

data Example = Example Int Int Int

example1 :: Example -> Int
example1 x = case x of
  Example a b c -> a + b + c

example2 :: Example -> Int
example2 (Example a b c) = a + b + c

fib :: Integer -> Integer
fib m = case m of
  0 -> 0
  1 -> 1
  n -> fib (n -1) + fib (n -2)

addOne :: [Int] -> [Int]
addOne [] = []
addOne (x : xs) = (x + 1) : (addOne xs)

{- Guards -}

absolute :: Int -> Int
absolute n
  | n < 0 = (- n)
  | otherwise = n

absoluteJust :: Maybe Int -> Maybe Int
absoluteJust n = case n of
  Nothing -> Nothing
  Just n
    | n < 0 -> Just (- n)
    | otherwise -> Just n

{- Operators and Sections -}

-- infixr 9 .
-- infixr 8 ^, ^^, **
-- infixl 7 *, /, `quot`, `rem`, `div`, `mod`
-- infixl 6 +, -
-- infixr 5 ++
-- infix 4 ==,/=,<,<=,>=,>
-- infixr 3 &&
-- infixr 2 ||
-- infixr 1 >>, >>=
-- infixr 0 $, `seq`
--  (+) x y = x + y

{- Tuples -}

-- fst :: (a, b) -> a
-- snd :: (a, b) -> b
tuple2 :: (Integer, String)
tuple2 = (1, "foo")

{- Where & Let Clauses -}

f :: Integer
f = let x = 1; y = 2 in (x + y)

f' :: Integer
f' = x + y where x = 1; y = 1

{- Conditionals -}

-- If statements are just syntactic sugar for case expressions over boolean values.
absolute' :: Int -> Int
absolute' n =
  if (n < 0)
    then (- n)
    else n

absolute'' :: Int -> Int
absolute'' n = case (n < 0) of
  True -> (- n)
  False -> n

{- Function Composition -}

example3 :: [Integer] -> [Integer]
example3 =
  sort
    . filter (< 100)
    . map (* 10)

-- ex1 = f1 . f2 . f3 . f4 $ input -- with ($)
-- ex1 = input & f1 . f2 . f3 . f4 -- with (&)
-- ex1 = (f1 . f2 . f3 . f4) input -- with explicit parens

sortSize :: [[a]] -> [[a]]
sortSize = sortBy (compare `on` length)

sorted :: [[Integer]]
sorted = sortSize [[1, 2], [1, 2, 3], [1]]

factorial :: Integer -> Integer
factorial n = product [1 .. n]

primes :: [Integer]
primes = sieve [2 ..]
  where
    sieve (p : xs) = p : sieve [n | n <- xs, n `mod` p > 0]

fizzbuzz :: [String]
fizzbuzz = [fb x | x <- [1 .. 100]]
  where
    fb y
      | y `mod` 15 == 0 = "FizzBuzz"
      | y `mod` 3 == 0 = "Fizz"
      | y `mod` 5 == 0 = "Buzz"
      | otherwise = show y

{- Typeclasses -}
class Equal a where
  equal :: a -> a -> Bool

instance Equal Bool where
  equal True True = True
  equal False False = True
  equal True False = False
  equal False True = False

data Ordering' = LT' | EQ' | GT'

instance Equal Ordering' where
  equal LT' LT' = True
  equal EQ' EQ' = True
  equal GT' GT' = True
  equal _ _ = False

instance (Equal a) => Equal [a] where
  equal [] [] = True
  equal [] ys = False
  equal xs [] = False
  equal (x : xs) (y : ys) = equal x y && equal xs ys

instance (Equal a, Equal b) => Equal (a, b) where
  equal (x0, x1) (y0, y1) = equal x0 y0 && equal x1 y1

{- List comprehension -}

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

{- Recursion -}

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

fib' :: (Eq t1, Num t1, Num t2) => t1 -> t2 -> t2 -> t2
fib' 0 x1 x2 = x2
fib' n x1 x2 = fib' (n - 1) x2 (x1 + x2)

fibnacci :: (Ord t1, Num t1, Num p) => t1 -> p
fibnacci n
  | n < 2 = 1
  | otherwise = fib' (n - 2) 1 1

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

{- Sorting -}

insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (y : ys)
  | x < y = x : y : ys
  | otherwise = y : insert' x ys

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x : xs) = insert' x (insertionSort xs)

-- tail recursion version
insertionSort' :: Ord a => [a] -> [a] -> [a]
insertionSort' xs [] = xs
insertionSort' xs (y : ys) = insertionSort' (insert' y xs) ys

swaps :: Ord a => [a] -> [a]
swaps [] = []
swaps [x] = [x]
swaps (x1 : x2 : xs)
  | x1 > x2 = x2 : swaps (x1 : xs)
  | otherwise = x1 : swaps (x2 : xs)

fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == x' then x else fix f x'
  where
    x' = f x

bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs = fix swaps xs

-- more efficient way
bubbleSort' :: Ord a => [a] -> [a]
bubbleSort' [] = []
bubbleSort' xs = bubbleSort' initialElements ++ [lastElement]
  where
    swappedxs = swaps xs
    initialElements = init swappedxs
    lastElement = last swappedxs

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (l : ls)
  | x == l = ls
  | otherwise = l : delete x ls

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = mini : selectionSort xs'
  where
    mini = minimum xs
    xs' = delete mini xs

filterSplit :: (a -> Bool) -> [a] -> ([a], [a])
filterSplit _ [] = ([], [])
filterSplit f (x : xs)
  | f x = ((x : l), r)
  | otherwise = (l, (x : r))
  where
    (l, r) = filterSplit f xs

quickSort' :: Ord a => [a] -> [a]
quickSort' [] = []
quickSort' [x] = [x]
quickSort' (x : xs) = quickSort' l ++ [x] ++ quickSort' r
  where
    (l, r) = filterSplit (< x) xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x > y = y : merge (x : xs) ys
  | otherwise = x : merge xs (y : ys)

msort :: Ord a1 => [a2] -> [a1]
msort xs = merge (msort x1) (msort x2)
  where
    (x1, x2) = halve xs
    halve xs = (take l xs, drop l xs)
    l = (length xs) `div` 2

{- Side Effects -}

-- Haskell’s approach to side effects is that they are encoded in the type system.
-- the language has various models for modeling these effects within the type system.
-- These models range from using Monads to building algebraic models of effects that draw clear lines between effectful code and pure code.

{- Records -}

--  Uses function application syntax since record accessors are simply just functions.

{- Bottoms ⊥ -}

-- 1. undefined
-- undefined :: a
-- mean :: Num a => Vector a -> a mean nums = (total / count)
--   where
--     total = undefined
--     count = undefined
-- addThreeNums :: Num a => a -> a -> a -> a addThreeNums n m j = undefined
-- f :: a -> Complicated Type f = undefined

-- 2. error
divByY :: (Num a, Eq a, Fractional a) => a -> a -> a
divByY _ 0 = error "Divide by zero error"
divByY dividend divisor = dividend / divisor

-- 3. infinitely loop
-- f' :: a
-- f' = let x = x in x

-- 4. A partial function that does not have exhaustive pattern matching defined
-- data F = A | B
-- case x of A -> ()
-- data Foo = Foo { example1 :: Int }
-- f = Foo {} -- Record defined with a missing field

-- Extract the first element of a list, which must be non-empty.
-- head :: [a] -> a
-- head(x:_) =x
-- head [] = error "Prelude.head: empty list"
-- (!!) :: [a] -> Int -> a
-- xs !! n | n < 0 = error "Prelude.!!: negative index"
-- [] !! _ = error "Prelude.!!: index too large"
-- (x : _) !! 0 = x
-- (_ : xs) !! n = xs !! (n - 1)

{- Exhaustiveness -}

-- {-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- {-# OPTIONS_GHC fwarn-incomplete-uni-patterns #-}
-- boom = \(Just a) -> something
-- boom2 = let
--   Just a = something
-- boom3 = do
--   Just a <- something

{- Debugger -}

-- λ: :set -fbreak-on-exception
-- λ: :break 2 15
-- λ: :trace main
-- λ: :hist
-- λ: :back λ: :forward
-- -- Sets option for evaluation to stop on exception
-- -- Sets a break point at line 2, column 15
-- -- Run a function to generate a sequence of evaluation steps
-- -- Step back from a breakpoint through previous evaluation steps -- Step backwards a single step at a time through the history
-- -- Step forward a single step at a time through the history

-- traceM :: (Monad m) => String -> m ()
-- traceM string = trace string $ return ()

-- traceShowM :: (Show a, Monad m) => a -> m ()
-- traceShowM = traceM . show

-- tracePrintfM :: (Monad m, PrintfArg a) => String -> a -> m ()
-- tracePrintfM s = traceM . printf s

{- Type Inference -}

-- Mutually Recursive Binding Groups
-- f x = const x g
-- g y = f 'A'
-- Polymorphic recursion
-- data Tree a = Leaf | Bin a (Tree (a, a))
-- size :: Tree a -> Int
-- size Leaf = 0
-- size (Bin _ t) = 1 + 2 * size t
-- Monomorphism Restriction

{- Type Holes -}

-- succ' :: _ => a -> a

{- Deferred Type Errors -}

-- {-# OPTIONS_GHC -fdefer-type-errors #-}

{- NameConventions -}

-- a,b,c.. Type level variable
-- x,y,z.. Value variables
-- f,g,h.. Higher order function values
-- x,y List head values xs,ys List tail values
-- m Monadic type variable
-- t Monad transformer variable
-- e Exception value
-- s Monad state value
-- r Monad reader value
-- t Foldable or Traversable type variable
-- f Functor or applicative type variable
-- mX Maybe variable

{- ghcid -}

-- ghcid --command="cabal repl" # Run cabal repl under ghcid
-- ghcid --command="stack repl" # Run stack repl under
-- ghcid ghcid --command="ghci baz.hs" # Open baz.hs under ghcid

{- HLint -}
{- DockerImages -}
{- ContinuousIntegration -}
{- Ormolu -}

{- Haddock -}

-- -- | Multiline documentation for the function -- f with multiple arguments.
-- fmap :: Functor f
--   => (a -> b) -- ^ function
--   -> f a -- ^ input
--   -> f b -- ^ output

{- UnsafeFunctions -}

-- Unsafe functions exist only for when one can manually prove the soundness of an expression
-- but can’t express this property in the type­system, or externalities to Haskell.
-- unsafeCoerce :: a -> b -- Unsafely coerce anything into anything
-- unsafePerformIO :: IO a -> a -- Unsafely run IO action outside of IO