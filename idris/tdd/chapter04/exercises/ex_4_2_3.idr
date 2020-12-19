data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

%name Vect xs, ys, zs

vectTake : (n : Nat) -> Vect (n + m) a -> Vect n a
vectTake Z [] = []
vectTake (S k) (x :: xs) = x :: vectTake k xs
