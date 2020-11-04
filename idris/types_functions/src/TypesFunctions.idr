module TypesFunctions

hello : IO ()
hello = do
  print "Hello, world!"

data Nat' = Z | S Nat'
data List' a = Nil | (::) a (List' a)

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

-- hole 

even' : Nat -> Bool
even' Z = True
even' (S k) = ?even_rhs