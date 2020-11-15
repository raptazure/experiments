module Lib
  ( someFunc,
  )
where

-- functor: computational context

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- fmap :: (a -> b) -> f a -> f b
-- make a type constructor an instance of Functor - kind * -> *
-- can't write `instance Functor Either where`: Either takes two parameters
-- instance Functor (Either a) where
--   fmap :: (b -> c) -> Either a b -> Either a c

-- instance Functor IO where
--   fmap f action = do
--     result <- action
--     return (f result)

-- (->) r : a type constructor that takes two type parameters
-- (->) r can be an instance of Functor

-- instance Functor ((->) r) where
--   fmap f g = (\x -> f (g x))

-- instance Functor (r ->) where
--   fmap f g = (\x -> f (g x))

-- fmap :: (a -> b) -> (r -> a) -> (r -> b)
-- function composition(?)
-- yes...
-- instance Functor ((->) r) where
--   fmap = (.)
-- fmap (*3) (+100) 1
-- (*3) `fmap` (+100) $ 1
-- (*3) . (+100) $ 1

-- fmap :: (a -> b) -> (f a -> f b)
-- we can think of fmap not as a function that takes one function and a functor and returns a functor,
-- but as a function that takes a function and returns a new function that's just like the old one, only it takes a functor as a parameter and returns a functor as the result.
-- It takes an a -> b function and returns a function f a -> f b. This is called lifting a function.

-- functor laws 1: fmap id = id
-- instance Functor Maybe where
--   fmap f (Just x) = Just (f x)
--   fmap f Nothing = Nothing

-- functor laws 2: fmap (f . g) = fmap f . fmap g
-- for any functor F, fmap (f . g) F = fmap f (fmap g F)

-- class (Functor f) => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

-- instance Applicative Maybe where
--   pure = Just
--   Nothing <*> _ = Nothing
--   (Just f) <*> something = fmap f something

-- (<$>) :: (Functor f) => (a -> b) -> f a -> f b
-- f <$> x = fmap f x

-- (++) <$> "johntra" <*> Just "volta"

-- instance Applicative [] where
--   pure x = [x]
--   fs <*> xs = [f x | f <- fs, x <- xs]
-- pure "Hey" :: Maybe String

-- [(+), (*)] <*> [1,2] <*> [3,4]
-- (++) <$> ["ha", "heh", "hmm"] <*> ["?", "!", "."]
-- [x * y | x <- [2, 5, 10], y <- [8, 10, 11]]
-- filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]

-- instance Applicative IO where
--   pure = return
--   a <*> b = do
--     f <- a
--     x <- b
--     return (f x)

-- myAction :: IO String
-- myAction = do
--   a <- getLine
--   b <- getLine
--   return $ a ++ b

-- myAction :: IO String
-- myAction = (++) <$> getLine <*> getLine

-- main :: IO ()
-- main = do
--   a <- (++) <$> getLine <*> getLine
--   putStrLn $ "The two lines concatenated turn out to be: " ++ a

{- Functions as Applicatives -}

-- instance Applicative ((->) r) where
--   pure x = (\_ -> x)
--   f <*> g = \x -> f x (g x)

-- (+) <$> (+ 3) <*> (* 100) $ 5

-- (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5

{- Applicative Laws -}
-- f <*> x = fmap f x
-- pure id <*> v = v
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- pure f <*> pure x = pure (f x)
-- u <*> pure y = pure ($ y) <*> u

{- Useful Functions for Applicatives -}
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

justList :: Maybe [Integer]
justList = fmap (\x -> [x]) (Just 4)

justList2 :: Maybe [Integer]
justList2 = liftA2 (:) (Just 3) (Just [4])

-- (:) <$> Just 3 <*> Just [4]

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (x : xs) = (:) <$> x <*> sequenceA' xs

l1 :: Maybe [Integer]
l1 = sequenceA' [Just 1, Just 2]

sequenceA'' :: (Applicative f) => [f a] -> f [a]
sequenceA'' = foldr (liftA2 (:)) (pure [])

nothing = sequenceA'' [Just 3, Nothing, Just 1]

sixFiveFour = sequenceA'' [(+ 3), (+ 2), (+ 1)] 3

true = and $ map (\f -> f 7) [(> 4), (< 10), odd]

sameTrue = and $ sequenceA'' [(> 4), (< 10), odd] 7

-- sequenceA [[1,2,3],[4,5,6]]
-- [[x,y] | x <- [1,2,3], y <- [4,5,6]]
-- sequenceA [[1,2],[3,4],[5,6]]
-- [[x,y,z] | x <- [1,2], y <- [3,4], z <- [5,6]]

-- sequenceA [getLine, getLine, getLine]
