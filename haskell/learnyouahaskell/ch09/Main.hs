import Control.Monad

-- import Data.Char

-- main = do
--   putStrLn "What's your first name?"
--   firstName <- getLine
--   putStrLn "What's your last name?"
--   lastName <- getLine
--   let bigFirstName = map toUpper firstName
--       bigLastName = map toUpper lastName
--   putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

-- main = do
--   line <- getLine
--   if null line
--     then return ()
--     else do
--       putStrLn $ reverseWords line
--       main

-- reverseWords :: String -> String
-- reverseWords = unwords . map reverse . words

-- main = do
--   -- return wrap value to IO actions
--   a <- return "hell"
--   b <- return "yeah"
--   putStrLn $ a ++ " " ++ b

-- main = do
--   let a = "hell"
--       b = "yeah"
--   putStrLn $ a ++ " " ++ b

-- putStr :: String -> IO ()
-- putStr [] = return ()
-- putStr (x : xs) = do
--   putChar x
--   putStr xs

-- print <=> putStrLn . show

-- main = do
--   print True
--   print 2
--   print "haha"
--   print 3.2
--   print [3, 4, 3]

-- main = do
--   c <- getChar
--   if c /= ' '
--     then do
--       putChar c
--       main
--     else return ()

-- main = do
--   c <- getChar
--   when (c /= ' ') $ do
--     putChar c
--     main

-- main = do
--   rs <- sequence [getLine, getLine, getLine]
--   -- sequence $ map print rs
--   -- mapM print rs
--   mapM_ print rs

-- main = forever $ do
--   putStr "Give me some input!"
--   l <- getLine
--   putStrLn $ map toUpper l

main = do
  colors <-
    forM
      [1, 2, 3, 4]
      ( \a -> do
          putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
          color <- getLine
          return color
      )
  putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
  -- forM colors putStrLn
  mapM putStrLn colors