module Prims

x : Int
x = 42

foo : String
foo = "Sausage machine"

bar : Char
bar = 'Z'

quux : Bool
quux = False


-- Unary addition
plus' : Nat -> Nat -> Nat
plus' Z     y = y
plus' (S k) y = S (plus' k y)

-- Unary multiplication
mult' : Nat -> Nat -> Nat
mult' Z     y = Z
mult' (S k) y = plus' y (mult' k y)

-- chat : IO ()
-- chat = do
--   putStr "Name: "
--   x <- getLine
--   let greet : String -> String
--     greet msg = msg ++ " " ++ x
--   putStrLn (greet "Hello")
--   putStrLn (greet "Bye")

data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

append : Vect n a -> Vect m a -> Vect (n + m) a
append [] y = y
append (x :: z) y = x :: append z y

