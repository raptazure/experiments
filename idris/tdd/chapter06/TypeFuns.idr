import Data.Vect

StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int

getStringOrInt : (isInt : Bool) -> StringOrInt isInt
getStringOrInt False = "Ninety four"
getStringOrInt True = 94

valToString : (isInt : Bool) -> StringOrInt isInt -> String
valToString False y = trim y
valToString True y = cast y

valToString' : (isInt : Bool) -> (case isInt of
                                        False => String
                                        True => Int) -> String
valToString' False y = trim y
valToString' True y = cast y

appendV : Vect n a -> Vect m a -> Vect (n + m) a 
appendV [] ys = ys
appendV (x :: xs) ys = x :: appendV xs ys

zipV : Vect n a -> Vect n b -> Vect n (a, b)
zipV [] ys = []
zipV (x :: xs) (y :: ys) = (x, y) :: zipV xs ys
