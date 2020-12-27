AdderType : (numargs : Nat) -> Type -> Type
AdderType Z numtype = numtype
AdderType (S k) numtype = (next : numtype) -> AdderType k numtype

adder : Num numtype => (numargs : Nat) -> numtype -> AdderType numargs numtype
adder Z acc = acc
adder (S k) acc = \next => adder k (next + acc)
