module Lib
  ( someFunc,
  )
where

import Control.Monad.State
  ( MonadState (get, put, state),
    State,
    filterM,
    foldM,
    join,
    liftM,
    runState,
  )
import Control.Monad.Writer
  ( MonadWriter (tell, writer),
    Product,
    Sum (Sum),
    Writer,
    runWriter,
  )
import qualified Data.ByteString as B
import Data.List
import Data.Ratio
import System.Random

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{- Writer Monad -}

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

notBig :: (Bool, String)
notBig = (3, "Smallish gang.") `applyLog` isBigGang

personNum :: (Int, String)
personNum = ("Bathcat", "Got outlaw name.") `applyLog` (\x -> (length x, "Applied length"))

-- not only string
-- applyLog' :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])

chi :: B.ByteString
chi = B.pack [99, 104] `mappend` B.pack [105]

applyLog' :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog' (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

type Food = String

type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

beer :: (Food, Sum Int)
beer = ("dogmeat", Sum 5) `applyLog'` addDrink `applyLog'` addDrink

-- newtype Writer w a = Writer {runWriter :: (a, w)}

-- instance (Monoid w) => Monad (Writer w) where
--   return x = Writer (x, mempty)
--   (Writer (x, v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

productDefault :: (Int, Product Int)
productDefault = runWriter (return 3 :: Writer (Product Int) Int)

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  tell ["Gonna multiply these two"]
  return (a * b)

multRes :: (Int, [String])
multRes = runWriter multWithLog

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = writer (a, ["Finished with " ++ show a])
  -- do
  -- tell ["Finished with " ++ show a]
  -- return a
  | otherwise = do
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    gcd' b (a `mod` b)

gcdRes :: Int
gcdRes = fst $ runWriter (gcd' 8 3)

logTrace :: IO ()
logTrace = mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)

-- Inenfficient list construction
-- gcd': a ++ (b ++ (c ++ (d ++ (e ++ f))))
-- gcdReverse: ((((a ++ b) ++ c) ++ d) ++ e) ++ f

-- `++` becomes left associative instead of right associative now
gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
  | b == 0 = do
    tell ["Finished with " ++ show a]
    return a
  | otherwise = do
    result <- gcdReverse b (a `mod` b)
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    return result

logReverseTrace :: IO ()
logReverseTrace = mapM_ putStrLn $ snd $ runWriter (gcdReverse 8 3)

-- Difference lists
-- [1,2,3] <=> \xs -> [1,2,3] ++ xs
-- f `append` g = \xs -> f (g xs)

newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
  (DiffList f) <> (DiffList g) = DiffList (\xs -> f (g xs))

instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> [] ++ xs)

-- (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

{- instance {- Fill here -} => Semigroup (Optional a) where
    (<>) = {- Fill here -}
instance {- Fill here -} => Monoid (Optional a) where
    mempty = {- Fill here -}
    -- There is a default implementation
    --     mappend = (<>)
    -- so you don't need to implement it.-}

-- mempty <=> id, mappend <=> function composition

gcdDiff :: Int -> Int -> Writer (DiffList String) Int
gcdDiff a b
  | b == 0 = do
    tell (toDiffList ["Finished with " ++ show a])
    return a
  | otherwise = do
    result <- gcdDiff b (a `mod` b)
    tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
    return result

resDl :: IO ()
resDl = mapM_ putStrLn . fromDiffList . snd . runWriter $ gcdDiff 110 34

-- comparing performance
finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
  tell (toDiffList ["0"])
finalCountDown x = do
  finalCountDown (x - 1)
  tell (toDiffList [show x])

countDownOne = mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown 500000

finalCountDown' :: Int -> Writer [String] ()
finalCountDown' 0 = do
  tell ["0"]
finalCountDown' x = do
  finalCountDown' (x -1)
  tell [show x]

countDownTwo = mapM_ putStrLn . snd . runWriter $ finalCountDown' 500000

{- Reader Monad -}
-- instance Monad ((->) r) where
--   return x = \_ -> x
--   h >>= f = \w -> f (h w) w

addStuff :: Integer -> Integer
addStuff = do
  a <- (* 2)
  b <- (+ 10)
  return (a + b)

addStuff' :: Integer -> Integer
addStuff' x =
  let a = (* 2) x
      b = (+ 10) x
   in a + b

nineteen = addStuff 3

-- applicative
nineteen' = (+) <$> (* 2) <*> (+ 10) $ 3

{- State Monad -}
type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x : xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a : xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack =
  let ((), newStack1) = push 3 stack
      (a, newStack2) = pop newStack1
   in pop newStack2

stackManip' :: Stack -> (Int, Stack)
stackManip' = do
  push 3
  a <- pop
  pop

-- newtype State s a = State {runState :: s -> (a, s)}

-- instance Functor (State s) where
--   fmap = liftM

-- instance Applicative (State s) where
--   pure x = State $ \s -> (x, s)
--   (<*>) = ap

-- instance Monad (State s) where
--   -- return x = State $ \s -> (x, s)
--   (State h) >>= f = State $ \s ->
--     let (a, newState) = h s
--         (State g) = f a
--      in g newState

pop' :: State Stack Int
pop' = state $ \(x : xs) -> (x, xs)

push' :: Int -> State Stack ()
push' a = state $ \xs -> ((), a : xs)

stackManip'' :: State Stack Int
stackManip'' = do
  push' 3
  -- a <- pop'
  pop'
  pop'

manipRes :: (Int, Stack)
manipRes = runState stackManip'' [5, 8, 2, 1]

stackStuff :: State Stack ()
stackStuff = do
  a <- pop'
  if a == 5
    then push' 5
    else do
      push' 3
      push' 8

stuffRes = runState stackStuff [9, 0, 2, 1, 0]

moreStack :: State Stack ()
moreStack = do
  a <- stackManip''
  if a == 100
    then stackStuff
    else return ()

-- MonadState typeclass: get put
-- get = state $ \s -> (s, s)
-- put newState = state $ \s -> ((), newState)

stackyStack :: State Stack ()
stackyStack = do
  stackNow <- get
  if stackNow == [1, 2, 3]
    then put [8, 3, 1]
    else put [9, 2, 1]

-- pop :: State Stack Int
-- pop = do
--   (x : xs) <- get
--   put xs
--   return x

-- push :: Int -> State Stack ()
-- push x = do
--   xs <- get
--   put (x : xs)

-- (>>=) :: State s a -> (a -> State s b) -> State s b
-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
-- It wouldn’t make sense to use >>= between two different monads. The monad is actually State s

-- Randomness and the State Monad
-- random :: (RandomGen g, Random a) => g -> (a, g)
randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a, b, c)

threeRes = runState threeCoins (mkStdGen 33)

{- Error Monad -}
-- not working now
-- instance (Error e) => Monad (Either e) where
--   return x = Right x
--   Right x >>= f = f x
--   Left err >>= f = Left err
--   fail msg = Left (strMsg msg)
-- Right 3 >>= \x -> return (x + 100) :: Either String Int

{- Monadic functions -}
-- liftM :: (Monad m) => (a -> b) -> m a -> m b
-- fmap :: (Functor f) => (a -> b) -> f a -> f b
-- applicative: <$> <=> fmap
-- liftM f m = m >>= (\x -> return (f x))
-- liftM f m = do
--   x <- m
--   return (f x)

liftTest = runState (liftM (+ 100) pop') [1, 2, 3, 4]

fmapTest = runState (fmap (+ 100) pop') [1, 2, 3, 4]

-- (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap mf m = do
  f <- mf
  x <- m
  return (f x)

-- Just (+3) <*> Just 4
-- Just (+3) `ap` Just 4

-- first write up a Monad instance, and then make an Applicative instance by just saying that pure is return and <*> is ap.
-- Similarly, if you already have a Monad instance for something, you can give it a Functor instance just by saying that fmap is liftM.

-- liftM2 is similar to liftA2
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = f <$> x <*> y

-- join :: (Monad m) => m (m a) -> m a
justNine = join (Just (Just 9))

joinedList = join [[1, 2, 3], [4, 5, 6]] -- concat

joinedWriter = runWriter $ join (writer (writer (1, "aaa"), "bbb"))

joinedEither = join (Right (Left "error")) :: Either String Int

joinedStateulComputation = runState (join (state $ \s -> (push' 10, 1 : 2 : s))) [0, 0, 0]

-- join :: (Monad m) => m (m a) -> m a
-- join mm = do
--   m <- mm
--   m

-- `m >>= f` is equal to `join (fmap f m)`

-- filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
-- filter :: (a -> Bool) -> [a] -> [a]

keepSmall :: Int -> Writer [String] Bool
keepSmall x
  | x < 4 = do
    tell ["Keeping " ++ show x]
    return True
  | otherwise = do
    tell [show x ++ " is too large, throwing it away"]
    return False

filterList :: [Int]
filterList = fst $ runWriter $ filterM keepSmall [9, 1, 5, 2, 10, 3]

filterLog :: IO ()
filterLog = mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9, 1, 5, 2, 10, 3]

-- getting a powerset is like getting all the combinations of keeping and throwing out elements from a set
powerset :: [a] -> [[a]]
powerset xs = filterM (\_ -> [True, False]) xs

-- foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
-- foldl :: (a -> b -> a) -> a -> [b] -> a

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
  | x > 9 = Nothing
  | otherwise = Just (acc + x)

justFourteen = foldM binSmalls 0 [2, 8, 3, 1]

nothing = foldM binSmalls 0 [2, 11, 3, 1]

{- Making a safe RPN calculator -}
readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of
  [(x, "")] -> Just x
  _ -> Nothing

-- readMaybe "1" :: Maybe Int
-- readMaybe "GO TO HELL" :: Maybe Int

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x : y : ys) "*" = return ((x * y) : ys)
foldingFunction (x : y : ys) "+" = return ((x + y) : ys)
foldingFunction (x : y : ys) "-" = return ((y - x) : ys)
foldingFunction xs numberString = liftM (: xs) (readMaybe numberString)

solveRPN :: String -> Maybe Double
solveRPN st = do
  [result] <- foldM foldingFunction [] (words st)
  return result

six = solveRPN "1 2 * 4 +"

nothing1 = solveRPN "1 2 * 4"

nothing2 = solveRPN "1 8 wharglbllargh"

{- Composing monadic functions -}
-- f = (+1) . (*100)
-- f 4
-- g = (\x -> return (x + 1)) <=< (\x -> return (x * 100))
-- Just 4 >>= g
-- f = foldr (.) id [(+1),(*100),(+1)]
-- f 1 => 201

-- inMany :: Int -> KnightPos -> [KnightPos]
-- inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)

-- canReachIn :: Int -> KnightPos -> KnightPos -> Bool
-- canReachIn x start end = end `elem` inMany x start

{- Making Monads -}
newtype Prob a = Prob {getProb :: [(a, Rational)]} deriving (Show)

instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

-- thisSituation :: Prob (Prob Char)
-- thisSituation =
--   Prob
--     [ (Prob [('a', 1 % 2), ('b', 1 % 2)], 1 % 4),
--       (Prob [('c', 1 % 2), ('d', 1 % 2)], 3 % 4)
--     ]

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAlt xs
  where
    multAlt (Prob innerxs, p) = map (\(x, r) -> (x, p * r)) innerxs

instance Applicative Prob where
  pure x = Prob [(x, 1 % 1)]
  (<*>) = ap

instance Monad Prob where
  m >>= f = flatten (fmap f m)

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads, 1 % 2), (Tails, 1 % 2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads, 1 % 10), (Tails, 9 % 10)]

flipThree :: Prob Bool
flipThree = do
  a <- coin
  b <- coin
  c <- loadedCoin
  return (all (== Tails) [a, b, c])

prob :: [(Bool, Rational)]
prob = getProb flipThree
