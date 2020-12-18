import Data.Vect

create_empties : Vect n (Vect 0 elem)
create_empties = replicate _ []

transpose_mat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transpose_mat [] = create_empties
transpose_mat (x :: xs) = let xs_trans = transpose_mat xs in
                            zipWith (::) x xs_trans

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

multVecs : Num a => (xs : Vect n a) -> (ys : Vect n a) -> a
multVecs xs ys = sum (zipWith (*) xs ys)

mkRow : Num a => (x : Vect n a) -> (ys_trans : Vect p (Vect n a)) -> Vect p a
mkRow x [] = []
mkRow x (y :: xs) = multVecs x y :: mkRow x xs

-- pay attention to the type of ys
multMatrix_helper : Num a => (xs : Vect m (Vect n a)) -> (ys : Vect p (Vect n a)) -> Vect m (Vect p a)
multMatrix_helper [] ys = []
multMatrix_helper (x :: xs) ys = mkRow x ys :: multMatrix_helper xs ys

multMatrix : Num a => Vect m (Vect n a) -> Vect n (Vect p a) -> Vect m (Vect p a)
multMatrix xs ys = let ys_trans = transpose_mat ys in 
                    multMatrix_helper xs ys_trans
