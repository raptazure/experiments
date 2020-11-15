module Main

import Data.Vect

data Nat' = Z | S Nat'
-- data List' a = Nil | (::) a (List' a)

-- data Nat : Type where
--   Z : Nat
--   S : Nat -> Nat

-- data List : Type -> Type where
--   Nil : List a
--   (::) : a -> List a -> List a

plus' : Nat -> Nat -> Nat
plus' Z     y = y
plus' (S k) y = S (plus' k y)

mult' : Nat -> Nat -> Nat
mult' Z     y = Z
mult' (S k) y = plus' y (mult' k y)

-- where

reverse : List a -> List a
reverse xs = revAcc [] xs where
  revAcc : List a -> List a -> List a
  revAcc acc [] = acc
  revAcc acc (x :: xs) = revAcc (x :: acc) xs


foo : Int -> Int
foo x = case isLT of
          Yes => x*2
          No => x*4
    where
      data MyLT = Yes | No

      isLT : MyLT
      isLT = if x < 20 then Yes else No

even : Nat -> Bool
even Z = True
even (S k) = odd k where
  odd Z = False
  odd (S k) = even k

-- -- hole

even' : Nat -> Bool
even' Z = True
even' (S k) = ?even_rhs

-- -- first class types

isSingleton : Bool -> Type
isSingleton True = Nat
isSingleton False = List Nat

mkSingle : (x : Bool) -> isSingleton x
mkSingle True = 0
mkSingle False = []

sum : (single : Bool) -> isSingleton single -> Nat
sum True x = x
sum False [] = 0
sum False (x :: xs) = x + sum False xs

-- -- vectors

-- data Vect : Nat -> Type -> Type where
--   Nil : Vect Z a
--   (::) : a -> Vect k a -> Vect (S k) a

-- (++) : Vect n a -> Vect m a -> Vect (n + m) a
-- (++) Nil       ys = ys
-- (++) (x :: xs) ys = x :: xs ++ ys

-- -- finite sets

data Fin' : Nat -> Type where
  FZ : Fin' (S k)
  FS : Fin' k -> Fin' (S k)

-- index : Fin n -> Vect n a -> a
-- index FZ (x :: xs) = x
-- index (FS k) (x :: xs) = index k xs

-- implicit arguments

-- index : Fin n -> Vect n a -> a
-- index : {a:Type} -> {n:Nat} -> Fin n -> Vect n a -> a
isEmpty : Vect n a -> Bool
isEmpty {n = Z} _   = True
isEmpty {n = S k} _ = False

data IsElem : a -> Vect n a -> Type where
  Here : {x:a} -> {xs:Vect n a} -> IsElem x (x :: xs)
  There : {x,y:a} -> {xs:Vect n a} -> IsElem x xs -> IsElem x (y :: xs)

testVec : Vect 4 Int
testVec = 3 :: 4 :: 5 :: 6 :: Nil

testVec' : Vect 0 Int
testVec' = Nil

inVect : IsElem 5 Main.testVec
inVect = There (There Here)

inVect' : IsElem 3 Main.testVec
inVect' = Here 

-- using 

using (x:a, y:a, xs:Vect n a)
  data IsElem' : a -> Vect n a -> Type where
    Here' : IsElem' x (x :: xs)
    There' : IsElem' x xs -> IsElem' x (y :: xs)

mutual
  even'' : Nat -> Bool
  even'' Z = True
  even'' (S k) = odd'' k

  odd'' : Nat -> Bool
  odd'' Z = False
  odd'' (S k) = even'' k

{- IO -}

-- data IO a

-- data File -- abstract
-- data Mode = Read | Write | ReadWrite

-- openFile : (f : String) -> (m : Mode) -> IO (Either FileError File)
-- closeFile : File -> IO ()

-- fGetLine : (h : File) -> IO (Either FileError String)
-- fPutStr : (h : File) -> (str : String) -> IO (Either FileError ())
-- fEOF : File -> IO Bool

-- do notation
-- pure : a -> IO a

doTest : IO ()
doTest = do 
  putStr "what is your name?"
  name <- getLine
  putStrLn ("hello, " ++ name)

-- laziness

ifThenElse' : Bool -> a -> a -> a
ifThenElse' True t e = t
ifThenElse' False t e = e

-- data Lazy : Type -> Type where
--   Delay : (val : a) -> Lazy a

-- Force : Lazy a -> a

-- A value of type Lazy a is unevaluated until it is forced by Force
-- The Idris type checker knows about the Lazy type, and inserts conversions where necessary between Lazy a and a, and vice versa.
-- Write ifThenElse as follows, without any explicit use of Force or Delay

ifThenElse'' : Bool -> Lazy a -> Lazy a -> a
ifThenElse'' True t e = t
ifThenElse'' False t e = e

-- codata types

-- codata Stream : Type -> Type where
--   (::) : (e : a) -> Stream a -> Stream a

-- translate to
-- data Stream : Type -> Type where
  -- (::) : (e : a) -> Inf (Stream a) -> Stream a

ones : Stream Nat
ones = 1 :: ones

-- codata does not allow the creation of infinite mutually recursive data structures

mutual
  data Blue : Type -> Type where
    B : a -> Inf (Red a) -> Blue a

  data Red : Type -> Type where
    R : a -> Inf (Blue a) -> Red a

mutual
  blue : Blue Nat
  blue = B 1 red

  red : Red Nat
  red = R 1 blue

mutual
  findB : (a -> Bool) -> Blue a -> a
  findB f (B x r) = if f x then x else findR f r

  findR : (a -> Bool) -> Red a -> a
  findR f (R x b) = if f x then x else findB f b

-- difference: coinductive type and inductive type

-- useful data types (part of Prelude.idr)

-- List and Vect
-- map : (a -> b) -> List a -> List b
-- map f []        = []
-- map f (x :: xs) = f x :: map f xs

-- map : (a -> b) -> Vect n a -> Vect n b
-- map f []        = []
-- map f (x :: xs) = f x :: map f xs

intVec : Vect 5 Int
intVec = [1, 2, 3, 4, 5]

double : Int -> Int
double x = x * 2

-- Maybe

-- data Maybe a = Just a | Nothing
list_lookup : Nat -> List a -> Maybe a
list_lookup _ Nil = Nothing
list_lookup Z (x::xs) = Just x
list_lookup (S k) (x::xs) = list_lookup k xs

maybe : Lazy b -> Lazy (a -> b) -> Maybe a -> b

data Pair a b = MkPair a b
fred : (String, Int)
fred = ("Fred", 42)

data DPair' : (a : Type) -> (P : a -> Type) -> Type where
  MkDPair' : {P : a -> Type} -> (x : a) -> P x -> DPair' a P

vec : (n : Nat ** Vect n Int)
vec = (2 ** [3, 4])

-- vec : DPair Nat (\n => Vect n Int)
-- vec = MkDPair 2 [3, 4]

filter' : (a -> Bool) -> Vect n a -> (p ** Vect p a)
filter' p Nil = (_ ** [])
filter' p (x :: xs) with (filter' p xs)
  | ( _ ** xs' ) = if (p x) then ( _ ** x :: xs' ) else ( _ ** xs' )

record Person where
  constructor MkPerson
  firstName, middleName, lastName : String
  age : Int

fred' : Person
fred' = MkPerson "Fred" "Joe" "Bloggs" 30
-- record { firstName = "Jim", age $= (+ 1) } fred'

record Class where
  constructor ClassInfo
  students : Vect n Person
  className : String

addStudent : Person -> Class -> Class
addStudent p c = record { students = p :: students c } c

addStudent' : Person -> Class -> Class
addStudent' p c = record { students $= (p ::) } c

record Prod a b where
  constructor Times
  fst : a
  snd : b

record SizedClass (size : Nat) where
  constructor SizedClassInfo
  students : Vect size Person
  className : String

addStudent'' : Person -> SizedClass n -> SizedClass (S n)
addStudent'' p c =  SizedClassInfo (p :: students c) (className c)

mirror : List a -> List a
mirror xs = let 
  xs' = Prelude.List.reverse xs 
    in
  xs ++ xs'

pythag : Int -> List (Int, Int, Int)
pythag n = [ (x, y, z) | z <- [1..n], y <- [1..z], x <- [1..y], x*x + y*y == z*z ]

splitAt : Char -> String -> (String, String)
splitAt c x = case 
  break (== c) x 
    of
  (x, y) => (x, strTail y)

lookup_default : Nat -> List a -> a -> a
lookup_default i xs def = case 
  list_lookup i xs 
    of
  Nothing => def
  Just x => x


main : IO ()
main = do
  doTest
  printLn $ findB (== 1) blue
  printLn $ map (\x => x * 2) intVec
  printLn $ map (* 10) intVec
