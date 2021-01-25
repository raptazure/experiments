checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat 0 0 = Just Refl
checkEqNat 0 (S k) = Nothing
checkEqNat (S k) 0 = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
                            Nothing => Nothing
                            Just prf => Just (cong S prf)
