printLength : IO ()
printLength = putStr "Input string: " >>= \_ => 
              getLine >>= \input => 
              putStrLn (show (length input))
