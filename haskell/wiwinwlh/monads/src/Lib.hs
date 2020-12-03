module Lib where

-- class Monad m where
--   return :: a -> m a
--   (>>=) :: m a -> (a -> m b) -> m b

(>>) :: Monad m => m a -> m b -> m b
m >> k = m >>= \_ -> k

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
