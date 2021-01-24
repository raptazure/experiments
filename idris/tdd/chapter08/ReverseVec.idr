import Data.Vect
import Data.Nat

myReverse : Vect n a -> Vect n a
myReverse [] = []
myReverse {n = S k} (x :: xs) =let res = myReverse xs ++ [x] in
                         rewrite plusCommutative 1 k in res

myReverse' : Vect n a -> Vect n a
myReverse' [] = []
myReverse' (x :: xs) = reverseProof (myReverse' xs ++ [x])
  where
    reverseProof : Vect (plus len 1) a -> Vect (S len) a
    reverseProof {len} result = rewrite plusCommutative 1 len in result
