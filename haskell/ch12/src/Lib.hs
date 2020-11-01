module Lib
  ( someFunc,
  )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- fmap :: (Functor f) => (a -> b) -> f a -> f b
-- (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

four :: Maybe Integer
four = Just 3 `applyMaybe` \x -> Just (x + 1)

nothing :: Maybe [Char]
nothing = Nothing `applyMaybe` \x -> Just (x ++ " :)")

-- class Monad m where
--   return :: a -> m a
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--   x >> y = x >>= \_ -> y
--   fail :: String -> m a
--   fail msg = error msg

-- return <=> pure, It takes a value and puts it in a minimal default context that still holds that value.

-- instance Monad Maybe where
--   return x = Just x
--   Nothing >>= f = Nothing
--   Just x >>= f = f x
--   fail _ = Nothing

what = return "what" :: Maybe String

mul :: Maybe Integer
mul = Just 9 >>= \x -> return (x * 10)

nothing2 :: Maybe Integer
nothing2 = Nothing >>= \x -> return (x * 10)

{- walk the line -}
type Birds = Int

type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise = Nothing

-- landLeft 2 (landRight 1 (landLeft 1 (0,0)))

x -: f = f x

-- (0,0) -: landLeft 1 -: landRight 1 -: landLeft 2

success :: Maybe Pole
success = return (0, 0) >>= landRight 2 >>= landLeft 2 >>= landRight 2

failed :: Maybe Pole
failed = return (0, 0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)

banana :: Pole -> Maybe Pole
banana _ = Nothing

slide :: Maybe Pole
slide = return (0, 0) >>= landLeft 1 >>= banana >>= landRight 1

-- (>>) :: (Monad m) => m a -> m b -> m b
-- m >> n = m >>= \_ -> n

-- λ> Nothing >> Just 3
-- Nothing
-- λ> Just 3 >> Just 4
-- Just 4
-- λ> Just 3 >> Nothing
-- Nothing

meantToFail :: Maybe Pole
meantToFail = return (0, 0) >>= landLeft 1 >> Nothing >>= landRight 1

-- if we hadn’t made the clever choice of treating Maybe values as values with a failure context and feeding them to functions
routine :: Maybe Pole
routine = case landLeft 1 (0, 0) of
  Nothing -> Nothing
  Just pole1 -> case landRight 4 pole1 of
    Nothing -> Nothing
    Just pole2 -> case landLeft 2 pole2 of
      Nothing -> Nothing
      Just pole3 -> landLeft 1 pole3

-- let x = 3; y = "!" in show x ++ y
foo :: Maybe String
foo =
  Just 3
    >>= ( \x ->
            Just "!"
              >>= ( \y ->
                      Just (show x ++ y)
                  )
        )

foo' :: Maybe String
foo' = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)

marySue :: Maybe Bool
marySue = Just 9 >>= (\x -> Just (x > 8))

marySue' :: Maybe Bool
marySue' = do
  x <- Just 9
  Just (x > 8)

routine' :: Maybe Pole
routine' = do
  start <- return (0, 0)
  first <- landLeft 2 start
  -- _ <- Nothing
  Nothing
  second <- landRight 2 first
  landLeft 1 second

-- pattern matching
justH :: Maybe Char
justH = do
  (x : xs) <- Just "hello"
  return x

-- Nothing (The failed pattern matching has caused a failure within the context of our monad instead of causing a program-wide failure)
wowop :: Maybe Char
wowop = do
  (x : xs) <- Just ""
  return x

{- List Monad -}

-- non-deterministic (we can view it as one value that is actually many values at the same time): list as applicative
-- (*) <$> [1,2,3] <*> [10,100,1000]

-- instance Monad [] where
--   return x = [x]
--   xs >>= f = concat (map f xs)
--   fail _ = []

-- concat [3,-3],[4,-4],[5,-5]]
opposite :: [Integer]
opposite = [3, 4, 5] >>= \x -> [x, - x]

-- Nothing
empty = [] >>= \x -> ["bad", "apple"]

-- [ (n,ch) | n <- [1,2], ch <- ['a','b'] ]
chain :: [(Integer, Char)]
chain = [1, 2] >>= \n -> ['a', 'b'] >>= \ch -> return (n, ch)

listOfTuples :: [(Int, Char)]
listOfTuples = do
  n <- [1, 2]
  ch <- ['a', 'b']
  return (n, ch)

-- do notation and list comprehensions are syntactic sugar for using lists as monads (>>=)

haveSeven :: [Integer]
haveSeven = [x | x <- [1 .. 50], '7' `elem` show x]

-- The MonadPlus type class is for monads that can also act as monoids.
class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

-- mzero: mempty, mplus: mappend (Monoid)
instance MonadPlus [] where
  mzero = []
  mplus = (++)

guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

[()] = guard (5 > 2) :: [()]

anotherFilter :: [Integer]
anotherFilter = [1 .. 50] >>= (\x -> guard ('7' `elem` show x) >> return x)

-- use >> to ignore that empty tuple and present something else as the result
["cool"] = guard (5 > 2) >> return "cool" :: [String]

-- [] >> return "cool"
nothingAgain = guard (1 > 2) >> return "cool" :: [String]

sevensOnly :: [Int]
sevensOnly = do
  x <- [1 .. 50]
  guard ('7' `elem` show x)
  return x

{- A knight's quest -}
type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
  (c', r') <-
    [ (c + 2, r -1),
      (c + 2, r + 1),
      (c -2, r -1),
      (c -2, r + 1),
      (c + 1, r -2),
      (c + 1, r + 2),
      (c -1, r -2),
      (c -1, r + 2)
      ]
  guard (c' `elem` [1 .. 8] && r' `elem` [1 .. 8])
  return (c', r')

-- moveKnight :: KnightPos -> [KnightPos]
-- moveKnight (c, r) =
--   filter
--     onBoard
--     [ (c + 2, r -1),
--       (c + 2, r + 1),
--       (c -2, r -1),
--       (c -2, r + 1),
--       (c + 1, r -2),
--       (c + 1, r + 2),
--       (c -1, r -2),
--       (c -1, r + 2)
--     ]
--   where
--     onBoard (c, r) = c `elem` [1 .. 8] && r `elem` [1 .. 8]

in3 :: KnightPos -> [KnightPos]
in3 start = do
  first <- moveKnight start
  second <- moveKnight first
  moveKnight second

in3' :: KnightPos -> [KnightPos]
in3' start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

canReach = (6, 2) `canReachIn3` (6, 1)