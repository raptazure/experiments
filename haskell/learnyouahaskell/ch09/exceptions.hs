import Control.Exception
import System.Directory
import System.Environment
import System.IO
import System.IO.Error

-- main = do
--   (fileName : _) <- getArgs
--   fileExists <- doesFileExist fileName
--   if fileExists
--     then do
--       contents <- readFile fileName
--       putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
--     else do putStrLn "The file doesn't exist!"

-- catch :: IO a -> (IOError -> IO a) -> IO a

main :: IO ()
main = toTry `catch` handler

toTry :: IO ()
toTry = do
  (fileName : _) <- getArgs
  contents <- readFile fileName
  putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler e
  | isDoesNotExistError e =
    case ioeGetFileName e of
      Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path
      Nothing -> putStrLn "Whoops! File does not exist at unknown location!"
  | otherwise = ioError e
