import Control.Applicative ()
import Data.Monoid ()

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