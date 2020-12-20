printTwoThings : IO ()
printTwoThings = do putStrLn "Hello"
                    putStrLn "World"

printInput : IO ()
printInput = do x <- getLine
                putStrLn x

printLength : IO ()
printLength = do 
  putStr "Input string: "
