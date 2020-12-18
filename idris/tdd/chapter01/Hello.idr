module Main

main : IO ()
main = putStrLn "Hello, idris world!"

dup : (x : a) -> (a, a)
dup x = (x, x)
