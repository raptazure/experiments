module Main

import System
import Data.Strings

countdown : (secs : Nat) -> IO ()
countdown Z = putStrLn "Lift off!"
countdown (S secs) = do putStrLn (show (S secs))
                        usleep 100000
                        countdown secs

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
      then pure (Just (stringToNatOrZ input))
      else pure Nothing

countdowns : IO ()
countdowns = 
  do putStr "Enter starting number: "
     Just starNum <- readNumber
      | Nothing => do putStrLn "Invalid input"
                      countdowns

     countdown starNum
     putStr "Another (y/n)?"
     yn <- getLine
     if yn == "y" then countdowns
                  else pure ()
