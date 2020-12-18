import Data.Vect

-- explicit.
append : (elem : Type) -> (n : Nat) -> (m : Nat) -> Vect n elem -> Vect m elem -> Vect (n + m) elem
append elem n m xs ys = ?append_rhs

-- unbound implicits
append' : Vect n elem -> Vect m elem -> Vect (n + m) elem
append' xs ys = ?append'_rhs

-- identifies that elem, n, m are undefined. rewrites the type. bound implicits
append'' : {elem: _} -> {n: _} -> {m: _} -> Vect n elem -> Vect m elem -> Vect (n + m) elem
append'' xs ys = ?append''_rhs

length' : Vect n elem -> Nat
length' [] = Z
length' (x :: xs) = 1 + length' xs

-- Type-level variables are implicit arguments to functions, which can be 
-- brought into scope and used like any other arguments by enclosing them in 
-- braces {}

-- {n} is a pattern brings the implicit argument n into scope
length'' : Vect n elem -> Nat
length'' {n} xs = n

-- partially apply append to its implcit arguments
-- append'' {elem = Char} {n = 2} {m = 3}

createEmpties : Vect n (Vect 0 a)
createEmpties {n = Z} = []
createEmpties {n = (S k)} = [] :: createEmpties

-- createEmpties {a = Int} {n = 4}
-- the (Vect 4 (Vect 0 Int)) createEmpties
