module Main where

import Control.Monad (when)
import qualified Data.ByteString as B
import Data.List
import Lib
import System.Environment
import System.Random

-- main :: IO ()
-- main = do
--   gen <- getStdGen
--   let randomChars = randomRs ('a', 'z') gen
--       (first20, rest) = splitAt 20 randomChars
--       (second20, _) = splitAt 20 rest
--   putStrLn first20
--   putStrLn second20

-- main = do
--     gen <- getStdGen
--     putStrLn $ take 20 (randomRs ('a','z') gen)
--     gen' <- newStdGen
--     putStr $ take 20 (randomRs ('a','z') gen')

-- main = do
--   gen <- getStdGen
--   let (randNumber, _) = randomR (1, 10) gen :: (Int, StdGen)
--   putStr "Which number in the range from 1 to 10 am I thinking of? "
--   numberString <- getLine
--   when (not $ null numberString) $ do
--     let number = read numberString
--     if randNumber == number
--       then putStrLn "You are correct!"
--       else putStrLn $ "Sorry, it was " ++ show randNumber
--     newStdGen
--     main

main = do
  gen <- getStdGen
  askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
  let (randNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)
  putStr "Which number in the range from 1 to 10 am I thinking of? "
  numberString <- getLine
  when (not $ null numberString) $ do
    let number = read numberString
    if randNumber == number
      then putStrLn "You are correct!"
      else putStrLn $ "Sorry, it was " ++ show randNumber
    askForNumber newGen

-- main = do
--   (fileName1 : fileName2 : _) <- getArgs
--   copyFile fileName1 fileName2

-- copyFile :: FilePath -> FilePath -> IO ()
-- copyFile source dest = do
--   contents <- B.readFile source
--   B.writeFile dest contents
