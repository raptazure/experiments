data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
  Same : (num : Nat) -> EqNat num num

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat 0 0 = Just (Same 0)
checkEqNat 0 (S k) = Nothing
checkEqNat (S k) 0 = Nothing
checkEqNat (S k) (S j) = do
  (Same k) <- checkEqNat k j
  Just (Same (S k))

exactLength : {m : _} -> (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = case checkEqNat m len of
                              Nothing => Nothing
                              Just (Same len) => Just input
