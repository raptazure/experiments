import Data.Vect

createEmpties : {n : _} -> Vect n (Vect 0 elem)
createEmpties {n = Z} = []
createEmpties {n = (S k)} = [] :: createEmpties

transposeHelper : (x : Vect n elem) -> (xs : Vect n (Vect k elem)) -> Vect n (Vect (S k) elem)
transposeHelper [] [] = []
transposeHelper (x :: ys) (y :: xs) = (x :: y) :: transposeHelper ys xs

transposeMat : {n : _} -> Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in 
                              transposeHelper x xsTrans
