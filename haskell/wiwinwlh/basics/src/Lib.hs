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