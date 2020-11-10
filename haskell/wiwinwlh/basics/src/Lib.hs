module Lib
  ( someFunc,
  )
where

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
