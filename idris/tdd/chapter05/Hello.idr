module Main

main : IO ()
main = do
  putStrLn "Enter your name: "
  x <- getLine
  putStrLn ("Hello, " ++ x ++ "!")
  