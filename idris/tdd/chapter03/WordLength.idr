import Data.Vect

allLengths : List String -> List Nat
allLengths [] = []
allLengths (x :: xs) = length x :: allLengths xs

total
allLengths' : Vect len String -> Vect len Nat
allLengths' [] = []
allLengths' (x :: xs) = length x :: allLengths' xs
