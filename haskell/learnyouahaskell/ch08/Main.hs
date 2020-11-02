import qualified Data.Map as Map

-- data Bool = False | True deriving (Ord)

data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

-- nudge (Circle (Point 34 34) 10) 5 10

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

-- module Shapes
-- ( Point(..)
-- , Shape(..)
-- , surface
-- , nudge
-- , baseCircle
-- , baseRect
-- ) where

data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int
  }
  deriving (Show, Eq, Read)

-- :t age

-- read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" == mikeD

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

-- Car {company="Ford", model="Mustang", year=1967}

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i * m) (j * m) (k * m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i * l + j * m + k * n

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- [minBound .. maxBound] :: [Day]
-- succ Monday

-- type String = [Char]

type PhoneNumber = String

type Name = String

type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

type AssocList k v = [(k, v)]

-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) ->
      if state /= Taken
        then Right code
        else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers =
  Map.fromList
    [ (100, (Taken, "ZD39I")),
      (101, (Free, "JAH3I")),
      (103, (Free, "IQSA9")),
      (105, (Free, "QOTSA")),
      (109, (Taken, "893JJ")),
      (110, (Taken, "99292"))
    ]

-- lockerLookup 100 lockers
-- lockerLookup 102 lockers

-- data List a = Empty | Cons {listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)

infixr 5 :-:

data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

-- 3 :-: 4 :-: 5 :-: Empty

-- infixr 5  ++
-- (++) :: [a] -> [a] -> [a]
-- []     ++ ys = ys
-- (x:xs) ++ ys = x : (xs ++ ys)

infixr 5 .++

(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

nums = [8, 6, 4, 1, 7, 3, 5]

numsTree = foldr treeInsert EmptyTree nums

insideTree = 1 `treeElem` numsTree

-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--   x == y = not (x /= y)
--   x /= y = not (x == y)

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"

-- class (Eq a) => Num a where

-- instance (Eq m) => Eq (Maybe m) where
--   Just x == Just y = x == y
--   Nothing == Nothing = True
--   _ == _ = False

-- :info Num

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  -- id return the same as input
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True

ysenoIf :: (YesNo y) => y -> a -> a -> a
ysenoIf yesnoVal yesResult noResult =
  if yesno yesnoVal then yesResult else noResult

-- functor typeclass (map over?)

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- instance Functor [] where
--   fmap = map

-- instance Functor Maybe where
--   fmap f (Just x) = Just (f x)
--   fmap f Nothing = Nothing

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x leftsub rightsub) =
    Node (f x) (fmap f leftsub) (fmap f rightsub)

mapTree = fmap (* 4) (foldr treeInsert EmptyTree [5, 7, 3, 2, 1, 7])

-- instance Functor (Either a) where
--   fmap f (Right x) = Right (f x)
--   fmap f (Left x) = Left x

-- :k Int

class Tofu t where
  tofu :: j a -> t a j

data Frank a b = Frank {frankField :: b a} deriving (Show)

instance Tofu Frank where
  tofu x = Frank x

-- tofu (Just 'a') :: Frank Char Maybe
-- tofu ["HELLO"] :: Frank [Char] []

data Barry t k p = Barry {yabba :: p, dabba :: t k}

instance Functor (Barry a b) where
  fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}
