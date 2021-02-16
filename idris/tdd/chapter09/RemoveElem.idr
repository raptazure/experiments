import Data.Vect
import Data.Vect.Elem
import Decidable.Equality

removeElem_v1 : DecEq a => (value : a) -> (xs : Vect (S n) a) -> Vect n a
removeElem_v1 value (x :: xs) = case decEq value x of
                                (Yes prf) => xs
                                (No contra) => ?removeElem_v1_rhs_3

removeElem : {n : _} -> (value : a) -> (xs : Vect (S n) a) ->
             {auto prf : Elem value xs} ->
             Vect n a
removeElem value (value :: ys) {prf = Here} = ys
removeElem {n = Z} value (y :: []) {prf = There later} = absurd later
removeElem {n = (S k)} value (y :: ys) {prf = There later}
                                          = y :: removeElem value ys
