totalLen : List String -> Nat
totalLen xs = foldr (\str, len => length str + len) 0 xs

totalLen' : List String -> Nat
totalLen' xs = foldl (\len, str => len + length str ) 0 xs

-- Foldable List where
--   foldr func acc [] = acc
--   foldr func acc (x :: xs) = func x (foldr func acc xs)

--   foldl func acc [] = acc
--   foldl func acc (x :: xs) = foldl func (func acc x) xs
