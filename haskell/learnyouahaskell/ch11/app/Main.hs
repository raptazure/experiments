module Main where

import Data.Char
import Data.List
import Lib

main :: IO ()
main = do
  -- fmap :: (a -> b) -> IO a -> IO b
  -- line <- fmap reverse getLine
  -- putStrLn $ "You said " ++ line ++ " backwards!"
  line <- fmap (intersperse '-' . reverse . map toUpper) getLine
  putStrLn line
