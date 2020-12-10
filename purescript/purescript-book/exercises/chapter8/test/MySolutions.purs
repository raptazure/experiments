module Test.MySolutions where

import Prelude
import Data.List (List(..))

-- class Functor f where
--   map :: forall a b. (a -> b) -> f a -> f b
-- class
--   Functor f <= Apply f where
--   apply :: forall a b. f (a -> b) -> f a -> f b
-- instance functorMaybe :: Functor Maybe where
--   map f (Just a) = Just (f a)
--   map f Nothing = Nothing
-- instance applyMaybe :: Apply Maybe where
--   apply (Just f) (Just x) = Just (f x)
--   apply _ _ = Nothing
lift3 :: forall a b c d f. Apply f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
lift3 f x y z = f <$> x <*> y <*> z

-- class
--   Apply f <= Applicative f where
--   pure :: forall a. a -> f a
-- instance applicativeMaybe :: Applicative Maybe where
--   pure x = Just x
combineList :: forall f a. Applicative f => List (f a) -> f (List a)
combineList Nil = pure Nil

combineList (Cons x xs) = Cons <$> x <*> combineList xs
