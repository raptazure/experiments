{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumDecimals #-}
-- {-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Lib
  ( someFunc,
    justOnes,
    combine,
    lookupDefault,
    headTup,
    headNil,
    first,
    second,
    postfixExample,
    bmiTell,
    lambdaCaseExample,
    googol,
    scope,
    assign,
    tapp,
    arr,
    elimTArr,
    tree,
    roseTree,
    printAny,
    test,
    example',
  )
where

-- import GHC.OverloadedLabels (IsLabel (..))
-- import GHC.Records (HasField (..))

import Control.Applicative (Applicative (liftA2), Const (Const))
-- import Data.Functor.Const (Const (..))
import Data.List (foldl1')
import Data.Text (Text)
import GHC.Exts (Any)
import GHC.Generics (Generic)
import Safe (headMay)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

justOnes :: Maybe [Int]
justOnes = do
  rec xs <- Just (1 : xs)
  return (Prelude.map negate xs)

combine :: (Eq a1, Num a2) => [(a1, a2)] -> a1 -> a1 -> Maybe a2
combine env x y
  | Just a <- lookup x env, Just b <- lookup y env = Just $ a + b
  | otherwise = Nothing

-- | View patterns are like pattern guards that can be nested inside of other patterns.
lookupDefault :: Eq a => a -> b -> [(a, b)] -> b
lookupDefault k _ (lookup k -> Just s) = s
lookupDefault _ d _ = d

headTup :: (a, [t]) -> [t]
headTup (headMay . snd -> Just n) = [n]
headTup _ = []

headNil :: [a] -> [a]
headNil (headMay -> Just x) = [x]
headNil _ = []

-- | The TupleSections syntax extension allows tuples to be constructed similar to how operator sections.
first :: a -> (a, Bool)
first = (,True)

second :: a -> (Bool, a)
second = (True,)

(!) :: Integer -> Integer
(!) n = product [1 .. n]

postfixExample :: Integer
postfixExample = (52 !)

-- | MultiWayIf allows us to write “pattern matching predicates” on a value, equivalent to a chain of if­ then­else statements.
bmiTell :: (Ord a, Fractional a) => a -> [Char]
bmiTell bmi =
  if
      | bmi <= 18.5 -> "Underweight."
      | bmi <= 25.0 -> "Average weight."
      | bmi <= 30.0 -> "Overweight."
      | otherwise -> "Clinically overweight."

-- | LambdaCase allows the elimination of redundant free variables introduced purely for the case of pattern matching on.
-- \temp -> case temp of
--   p1 -> 32
--   p2 -> 32

-- \case
--   p1 -> 32
--   p2 -> 32

data Exp a = Lam a (Exp a) | Var a | App (Exp a) (Exp a)

lambdaCaseExample :: Exp p -> p
lambdaCaseExample = \case
  Lam a _ -> a
  Var a -> a
  App a _ -> lambdaCaseExample a

-- | NumDecimals allows the use of exponential notation for integral literals that are not necessarily floats.
-- Without it, any use of exponential notation induces a Fractional class constraint.
googol :: Num a => a
googol = 1e100

-- | PackageImports
-- Explicitly ask GHC to resolve that Control.Monad.Error package be drawn from the mtl library.
-- import qualified "mtl" Control.Monad.Error as Error
-- import qualified "mtl" Control.Monad.State as State
-- import qualified "mtl" Control.Monad.Reader as Reader

-- | RecordWildCards
data Example = Example
  { e1 :: Int,
    e2 :: Text,
    e3 :: Text
  }
  deriving (Show)

-- Extracting from a record using wildcards
scope :: Example -> (Int, Text, Text)
scope Example {..} = (e1, e2, e3)

-- Assign to a record using wildcards.
assign :: Example
assign = Example {..}
  where
    (e1, e2, e3) = (1, "Kirk", "Picard")

-- | NamedFieldPuns provides alternative syntax for accessing record fields in a pattern match.

-- data D = D {a :: Int, b :: Int}
-- f :: D -> Int
-- f D {a, b} = a - b

-- Order doesn't matter
-- g :: D -> Int
-- g D {b, a} = a - b

-- | PatternSynonyms
type TVar = String

type TyCon = String

data Type
  = TVar TVar
  | TCon TyCon
  | TApp Type Type
  deriving (Show, Eq, Ord)

pattern TArr :: Type -> Type -> Type
pattern TArr t1 t2 = TApp (TApp (TCon "(->)") t1) t2

tapp :: TyCon -> [Type] -> Type
tapp tcon = foldl TApp (TCon tcon)

arr :: [Type] -> Type
arr = foldl1' (\t1 t2 -> tapp "(->)" [t1, t2])

elimTArr :: Type -> [Type]
elimTArr (TArr (TArr t1 t2) t3) = t1 : t2 : elimTArr t3
elimTArr (TArr t1 t2) = t1 : elimTArr t2
elimTArr t = [t]

to :: Type
to = arr [TVar "a", TVar "b", TVar "a"]

from :: [Type]
from = elimTArr Lib.to

-- module MyModule (
--   pattern Elt
-- ) where
-- pattern Elt = [a]

-- | DeriveFunctor
data Tree a = Node a [Tree a] deriving (Show, Functor)

tree :: Tree Int
tree = fmap (+ 1) (Node 1 [Node 2 [], Node 3 []])

-- | DeriveFoldable: many instances of Foldable for types of kind * -> * have instances that derive the functions: `foldMap`, `foldr`, `null`
data Tree' a = Leaf a | Branch (Tree' a) (Tree' a) deriving (Foldable)

-- will generate the following instances

-- instance Foldable Tree' where
--   foldr f z (Leaf a1) = f a1 z
--   foldr f z (Branch a1 a2) =
--     (\b1 b2 -> foldr f b2 b1) a1 ((\b3 b4 -> foldr f b4 b3) a2 z)
--   foldMap f (Leaf a1) = f a1
--   foldMap f (Branch a1 a2) = mappend (foldMap f a1) (foldMap f a2)
--   null (Leaf _) = False
--   null (Branch a1 a2) = (&&) (null a1) (null a2)

-- | DeriveTraversable: many instances for single­paramater datatypes of kind * -> * have trivial implementations of the function which can also be derived automatically.
data RoseTree a = RoseNode a [RoseTree a] deriving (Show, Functor, Foldable, Traversable)

roseTree :: Maybe [Int]
roseTree = foldMap go (RoseNode [1] [RoseNode [2] [], RoseNode [3, 4] []])
  where
    go [] = Nothing
    go xs = Just xs

-- | DeriveGeneric
data List a
  = Cons a (List a)
  | Nil
  deriving (Generic)

-- | DeriveAnyClass
class MinimalClass a where
  const1 :: a -> Int
  default const1 :: a -> Int
  const1 _ = 1
  const2 :: a -> Int
  default const2 :: a -> Int
  const2 _ = 2

data Example' = Example' deriving (MinimalClass)

printAny :: IO ()
printAny = do
  print $ const1 Example'
  print $ const2 Example'

-- | DuplicateRecordFields
data Person = Person {id :: Int}

data Animal = Animal {id :: Int}

data Vegetable = Vegetable {id :: Int}

test :: (Person, Animal, Vegetable)
test = (Person {id = 1}, Animal {id = 2}, Vegetable {id = 3})

-- | OverloadedLabels: allows a limited form of polymorphism over labels that share the same name.
data S = MkS {foo :: Int}

data T x y z = forall b. MkT {foo :: y, bar :: b}

-- instance HasField x r a => IsLabel x (r -> a) where
--   fromLabel = getField

-- printLable :: IO ()
-- printLable = do
--   print (#foo (MkS 42))
--   print (#foo (MkT True False))

-- | TypeApplications allows you to use explicit annotations for subexpressions.
--  This is particularly useful when working with typeclasses where type inference cannot deduce the types of all subexpressions from the toplevel signature and results in an overly specific default.
-- This is quite common when working with roundtrips of read and show .

-- a :: Proxy Int
-- a = Proxy @Int

b :: String
b = show (read @Int "42")

-- | DerivingVia is an extension of GeneralizedNewtypeDeriving

-- Deriving Eq in terms of Const functor
newtype Age = MkAge Int
  deriving
    (Eq)
    via Const Int Any

-- Deriving Num across a nested functor
newtype FNum f a = FNum (f a)
  deriving stock (Functor)
  deriving newtype (Applicative)

instance (Applicative f, Num a) => Num (FNum f a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = FNum . pure . fromInteger

newtype Example'' a b = Example'' (Either a b)
  deriving stock (Show, Functor)
  deriving newtype (Applicative)
  deriving (Num) via FNum (Either a) b

a :: Example'' Integer Integer
a = Example'' (Left 1)

b' :: Example'' Integer Integer
b' = Example'' (Right 1)

example' :: IO ()
example' = do
  print (a + a)
  print (a + b')
  print (b' + b')

-- | DerivingStrategies allows you to disambiguate which algorithm GHC should use for individual class derivations.
-- stock: Standard GHC builtin deriving (i.e. Eq , Ord , Show)
-- anyclass: Deriving via minimal annotations with DeriveAnyClass.
-- newtype: Deriving with [GeneralizedNewtypeDeriving].
-- via: Deriving with DerivingVia.
