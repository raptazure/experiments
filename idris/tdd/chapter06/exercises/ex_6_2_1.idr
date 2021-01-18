import Data.Vect

Matrix : Nat -> Nat -> Type
Matrix k j = Vect k (Vect j Double)

testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]
