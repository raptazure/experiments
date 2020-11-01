import Control.Applicative ()
import qualified Data.Foldable as F
import Data.Monoid (Any (Any, getAny))

{- newtype -}

-- getZipList $ ZipList [(+1), (*100), (*5)] <*> ZipList [1,2,3] $

-- When you make a new type from an existing type by using the newtype keyword
-- you can have only one value constructor, and that value constructor can have only one field.
-- newtype ZipList a = ZipList {getZipList :: [a]}

-- newtype CharList = CharList {getCharList :: [Char]} deriving (Eq, Show)
-- CharList :: [Char] -> CharList
-- getCharList :: CharList -> [Char]

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- instance Functor Maybe where
--   fmap :: (a -> b) -> Maybe a -> Maybe b

newtype Pair b a = Pair {getPair :: (a, b)}

instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)

-- getPair $ fmap (*100) (Pair (2, 3))

-- on newtype laziness
newtype CoolBool = CoolBool {getCoolBool :: Bool}

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

-- helloMe undefined

{- Monoid -}

-- class Monoid m where
--   mempty :: m
--   mappend :: m -> m -> m
--   mconcat :: [m] -> m
--   mconcat = foldr mappend mempty

-- the m in the type class definition doesn’t take any type parameters.
-- This is different from Functor and Applicative,
-- which require their instances to be type constructors that take one parameter.

-- • mempty `mappend` x = x
-- • x `mappend` mempty = x
-- • (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

-- List are Monoids
-- instance Monoid [a] where
--   mempty = []
--   mappend = (++)

-- product and sum
-- newtype Product a = Product {getProduct :: a}
--   deriving (Eq, Ord, Read, Show, Bounded)

-- instance Num a => Monoid (Product a) where
--   mempty = Product 1
--   Product x `mappend` Product y = Product (x * y)

-- getProduct . mconcat . map Product $ [3, 4, 2]
-- getProduct $ Product 3 `mappend` Product 4 `mappend` Product 2

-- newtype Any = Any {getAny :: Bool}

-- instance Monoid Any where
--   mempty = Any False
--   Any x `mappend` Any y = Any (x || y)

-- getAny . mconcat . map Any $ [False, False, False, True]

-- newtype All = All {getAll :: Bool}
--   deriving (Eq, Ord, Read, Show, Bounded)

-- instance Monoid All where
--   mempty = All True
--   All x `mappend` All y = All (x && y)

-- getAll $ mempty `mappend` All False
-- getAll $ mempty `mappend` All True

-- instance Monoid Ordering where
--   mempty = EQ
--   LT `mappend` _ = LT
--   EQ `mappend` y = y
--   GT `mappend` _ = GT

lengthCompare :: String -> String -> Ordering
lengthCompare x y =
  let a = length x `compare` length y
      b = x `compare` y
   in if a == EQ then b else a

lengthCompare' :: String -> String -> Ordering
lengthCompare' x y = (length x `compare` length y) `mappend` (x `compare` y)

lengthCompare'' :: String -> String -> Ordering
lengthCompare'' x y = (length x `compare` length y) `mappend` (vowels x `compare` vowels y) `mappend` (x `compare` y)
  where
    vowels = length . filter (`elem` "aeiou")

-- instance Monoid a => Monoid (Maybe a) where
--   mempty = Nothing
--   Nothing `mappend` m = m
--   m `mappend` Nothing = m
--   Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

-- Just LT `mappend` Nothing
-- Nothing `mappend` Just "andy"
-- Just (Sum 3) `mappend` Just (Sum 4)

newtype First' a = First' {getFirst :: Maybe a}
  deriving (Eq, Ord, Read, Show)

instance Semigroup (First' a) where
  First' Nothing <> b = b
  a <> _ = a

instance Monoid (First' a) where
  mempty = First' Nothing
  First' (Just x) `mappend` _ = First' (Just x)
  First' Nothing `mappend` x = x

-- getFirst $ First (Just 'a') `mappend` First (Just 'b')
-- getFirst $ First Nothing `mappend` First (Just 'b')
-- getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]
--  getLast . mconcat . map Last $ [Nothing, Just 9, Just 10]

-- foldr :: (a -> b -> b) -> b -> [a] -> b (Prelude)
-- F.foldr :: (F.Foldable t) => (a -> b -> b) -> b -> t a -> b (Foldable)

--  F.foldl (+) 2 (Just 9)
--  F.foldr (||) False (Just True)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m

instance F.Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Node x l r) = F.foldMap f l `mappend` f x `mappend` F.foldMap f r

testTree =
  Node
    5
    ( Node
        3
        (Node 1 Empty Empty)
        (Node 6 Empty Empty)
    )
    ( Node
        9
        (Node 8 Empty Empty)
        (Node 10 Empty Empty)
    )

plusFold :: Int
plusFold = F.foldl (+) 0 testTree

productFold :: Int
productFold = F.foldl (*) 1 testTree

haveThree :: Bool
haveThree = getAny $ F.foldMap (\x -> Any $ x == 3) testTree

haveBigger :: Bool
haveBigger = getAny $ F.foldMap (\x -> Any $ x > 15) testTree

treeList :: [Int]
treeList = F.foldMap (\x -> [x]) testTree