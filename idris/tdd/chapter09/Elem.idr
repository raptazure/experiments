import Data.Vect
import Data.Vect.Elem

data Elem' : a -> Vect k a -> Type where
  Here : Elem' x (x :: xs)
  There : (later : Elem' x xs) -> Elem' x (y :: xs)

oneInVect : Elem 1 [1, 2, 3]
oneInVect = Here

maryInVector : Elem "Mary" ["Peter", "Paul", "Mary"]
maryInVector = There (There Here)

fourNotInVector : Elem 4 [1,2,3] -> Void
fourNotInVector (There (There (There Here))) impossible
fourNotInVector (There (There (There (There _)))) impossible

peteNotInVector : Elem "Pete" ["John", "Paul", "George", "Ringo"] -> Void
peteNotInVector (There (There (There (There Here)))) impossible
peteNotInVector (There (There (There (There (There _))))) impossible
