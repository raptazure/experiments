module Lib where

import Control.Monad.Reader
import Control.Monad.Writer

-- class Monad m where
--   return :: a -> m a
--   (>>=) :: m a -> (a -> m b) -> m b

-- (>>) :: Monad m => m a -> m b -> m b
-- m >> k = m >>= \_ -> k

-- | Maybe Monad

-- instance Monad Maybe where
--   (Just x) >>= k = k x
--   Nothing >>= k = Nothing
--   return = Just

maybeExample1 :: Maybe Int
maybeExample1 = do
  a <- Just 3
  b <- Just 4
  return $ a + b

maybeDesugared1 :: Maybe Int
maybeDesugared1 = Just 3 >>= \a -> Just 4 >>= \b -> return $ a + b

-- | List Monad

-- instance Monad [] where
--   m >>= f = concat (map f m)
--   return x = [x]

-- a = [f x y | x <- xs, y <- ys, x == y]
-- b = do
--   x <- xs
--   y <- ys
--   guard $ x == y
--   return $ f x y

listExample :: [(Int, Int, Int)]
listExample = do
  a <- [1, 2]
  b <- [10, 20]
  c <- [100, 200]
  return (a, b, c)

listDesugared :: [(Int, Int, Int)]
listDesugared =
  [1, 2] >>= \a ->
    [10, 20] >>= \b ->
      [100, 200] >>= \c ->
        return (a, b, c)

-- | IO Monad
namer :: IO ()
namer = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn name

-- namer' :: IO ()
-- namer' = putStrLn "What is your name:" >>=
-- \_ -> getLine >>= \name -> putStrLn name

namer' :: IO ()
namer' =
  putStrLn
    "What is your name?"
    >> ( getLine
           >>= putStrLn
       )

{- what is the point -}
sequence' :: Monad m => [m a] -> m [a]
sequence' = foldr mcons (return [])

mcons :: Monad m => m t -> m [t] -> m [t]
mcons p q = do
  x <- p
  y <- q
  return (x : y)

test1 :: Maybe [Integer]
test1 = sequence' [Just 3, Just 4]

test2 :: [[Integer]]
test2 = sequence' [[1, 2, 3], [4, 5, 6]]

test3 :: IO [String]
test3 = sequence' [getLine, getLine, getLine]

{- Reader Monad -}
-- newtype Reader r a = Reader {runReader :: r -> a}

-- instance Monad (Reader r) where
--   return a = Reader $ \_ -> a
--   m >>= k = Reader $ \r -> runReader (k (runReader m r)) r

-- ask :: Reader a a
-- ask = Reader id

-- asks :: (r -> a) -> Reader r a
-- asks f = Reader f

-- local :: (r -> r) -> Reader r a -> Reader r a
-- local f m = Reader $ runReader m . f

data MyContext = MyContext
  { foo :: String,
    bar :: Int
  }
  deriving (Show)

computation :: Reader MyContext (Maybe String)
computation = do
  n <- asks bar
  x <- asks foo
  if n > 0
    then return (Just x)
    else return Nothing

ex1 :: Maybe String
ex1 = runReader computation $ MyContext "hello" 0

ex2 :: Maybe String
ex2 = runReader computation $ MyContext "haskell" 1

{- Writer Monad -}
-- newtype Writer w a = Writer {runWriter :: (a, w)}

-- instance Monoid w => Monad (Writer w) where
--   return a = Writer (a, mempty)
--   m >>= k =
--     Writer $
--       let (a, w) = runWriter m
--           (b, w') = runWriter (k a)
--        in (b, w `mappend` w')

-- execWriter :: Writer w a -> w
-- execWriter m = snd (runWriter m)

-- tell :: w -> Writer w ()
-- tell w = Writer ((), w)

type MyWriter = Writer [Int] String

writerExample :: MyWriter
writerExample = do
  tell [1 .. 3]
  tell [3 .. 5]
  return "foo"

output :: (String, [Int])
output = runWriter writerExample

{- State Monad -}
-- newtype State s a = State {runState :: s -> (a, s)}

-- instance Monad (State s) where
--   return a = State $ \s -> (a, s)
--   State act >>= k = State $ \s ->
--     let (a, s') = act s
--      in runState (k a) s'

-- get :: State a a
-- get = State $ \s -> (s, s)

-- put :: p -> State s ()
-- put s = State $ \s -> ((), s)

-- modify :: (s -> s) -> State s ()
-- modify f = get >>= \x -> put (f x)

-- evalState :: State b c -> b -> c
-- evalState act = fst . runState act

-- execState :: State c a -> c -> c
-- execState act = snd . runState act
