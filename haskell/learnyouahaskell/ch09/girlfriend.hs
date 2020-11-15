-- import Data.Char
import Data.List
import System.Directory
import System.IO

-- type FilePath = String
-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
-- openFile :: FilePath -> IOMode -> IO Handle

main :: IO ()
-- main = do
--   handle <- openFile "girlfriend.txt" ReadMode
--   contents <- hGetContents handle
--   putStr contents
--   hClose handle

-- withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a

-- main = do
--   withFile
--     "girlfriend.txt"
--     ReadMode
--     ( \handle -> do
--         contents <- hGetContents handle
--         putStr contents
--     )

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
  handle <- openFile path mode
  result <- f handle
  hClose handle
  return result

-- readFile :: FilePath -> IO String

-- main = do
--   contents <- readFile "girlfriend.txt"
--   putStr contents

-- writefile :: FilePath -> String -> IO ()

-- main = do
--   contents <- readFile "girlfriend.txt"
--   writeFile "girlfriendcaps.txt" (map toUpper contents)

-- main = do
--   todoItem <- getLine
--   appendFile "todo.txt" (todoItem ++ "\n")

-- main = do
--   withFile
--     "haiku.txt"
--     ReadMode
--     ( \handle -> do
--         hSetBuffering handle $ BlockBuffering (Just 2048)
--         contents <- hGetContents handle
--         putStr contents
--     )

main = do
  handle <- openFile "todo.txt" ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
  putStrLn "These are your TO-DO items:"
  putStr $ unlines numberedTasks
  putStrLn "Which one do you want to delete?"
  numberString <- getLine
  let number = read numberString
      newTodoItems = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile "todo.txt"
  renameFile tempName "todo.txt"
