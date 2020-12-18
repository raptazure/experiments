palindrome_q3 : String -> Bool
palindrome_q3 str = 
  let strL = toLower str in
    strL == reverse strL

palindrome_q4 : String -> Bool
palindrome_q4 str = 
  if length str > 10
  then palindrome_q3 str
  else False

palindrome_q5 : Nat -> String -> Bool
palindrome_q5 min str = 
  if length str > min
  then palindrome_q3 str
  else False

counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

top_ten : Ord a => List a -> List a
top_ten xs = take 10 (reverse (sort xs))

over_length : Nat -> List String -> Nat
over_length num xs = 
  let lengths = map length xs in
    length (filter (> num) lengths)

countIO : IO ()
countIO = repl "Enter a string: " show_counts 
  where
    show_counts : String -> String
    show_counts x = show (counts x) ++ "\n"
