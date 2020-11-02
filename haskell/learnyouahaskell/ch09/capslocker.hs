-- import Control.Monad
import Data.Char

-- main = forever $ do
--   putStr "Give me some input: "
--   l <- getLine
--   putStrLn $ map toUpper l

-- cat haiku.txt | ./capslocker

-- main :: IO ()
-- main = do
--   contents <- getContents
--   putStr (map toUpper contents)

-- main :: IO ()
-- main = do
--   contents <- getContents
--   putStr (shortLinesOnly contents)

-- shortLinesOnly :: String -> String
-- shortLinesOnly input =
--   let allLines = lines input
--       shortLines = filter (\line -> length line < 10) allLines
--       result = unlines shortLines
--    in result

-- main :: IO ()
-- main = interact shortLinesOnly

-- shortLinesOnly :: String -> String
-- shortLinesOnly input =
--   let allLines = lines input
--       shortLines = filter (\line -> length line < 10) allLines
--       result = unlines shortLines
--    in result

-- main :: IO ()
-- main = interact $ unlines . filter ((< 10) . length) . lines

-- respondPalindromes contents =
--   unlines
--     ( map
--         ( \xs ->
--             if isPalindrome xs then "palindrome" else "not a palindrome"
--         )
--         (lines contents)
--     )
--   where
--     isPalindrome xs = xs == reverse xs

respondPalindromes :: String -> String
respondPalindromes =
  unlines
    . map
      ( \xs ->
          if isPalindrome xs then "palindrome" else "not a palindrome"
      )
    . lines
  where
    isPalindrome xs = xs == reverse xs

main :: IO ()
main = interact respondPalindromes
