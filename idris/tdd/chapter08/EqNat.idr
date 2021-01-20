data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
  Same : (num : Nat) -> EqNat num num

sameS : (k : Nat) -> (j : Nat) -> (eq : EqNat k j) -> EqNat (S k) (S j)
sameS k k (Same k) = Same (S k)

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat 0 0 = Just (Same 0)
checkEqNat 0 (S k) = Nothing
checkEqNat (S k) 0 = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of 
                            Nothing => Nothing
                            Just eq => Just (sameS k j eq)

checkEqNat' : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat' 0 0 = Just (Same 0)
checkEqNat' 0 (S k) = Nothing
checkEqNat' (S k) 0 = Nothing
checkEqNat' (S k) (S j) = do
  (Same k) <- checkEqNat' k j
  Just (Same (S k))

typeof : {t : Type} -> t -> String
typeof {t} _ with (t)
  typeof _ | String = "string"
  typeof _ | Integer = "number"
  typeof _ | Double = "number"
  typeof _ | Bool = "boolean"
  typeof _ | _ = "object"
