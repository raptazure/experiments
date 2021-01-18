import Data.Vect
import Data.Strings

-- type-level functions exist at compile time only
-- only total functions will be evaluated at type level
StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int

getStringOrInt : (isInt : Bool) -> StringOrInt isInt
getStringOrInt False = "Ninety four"
getStringOrInt True = 94

valToString : (isInt : Bool) -> StringOrInt isInt -> String
valToString False x = trim x
valToString True x = cast x

valToString' : (isInt : Bool) -> (case isInt of
                                       False => String
                                       True => Int) -> String
valToString' False y = trim y
valToString' True y = cast y
