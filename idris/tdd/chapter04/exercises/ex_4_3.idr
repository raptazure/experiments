module Main
import Data.Vect

data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore

size : DataStore -> Nat
size (MkData size items) = size

items : (store : DataStore) -> Vect (size store) String
items (MkData size items) = items

addToStore : DataStore -> String -> DataStore
addToStore (MkData size store) newitem = MkData _ (addToData store)
  where 
    addToData : Vect oldsize String -> Vect (S oldsize) String
    addToData [] = [newitem]
    addToData (x :: xs) = x :: addToData xs

data Command = 
  Add String
  | Get Integer
  | Size
  | Search String
  | Quit

parseCommand : List String -> Maybe Command
parseCommand ("add" :: rest) = Just (Add (unwords rest))
parseCommand ["get", val] = case all isDigit (unpack val) of
                                 False => Nothing
                                 True => Just (Get (cast val))
parseCommand ["quit"] = Just Quit
parseCommand ["size"] = Just Size
parseCommand ("search" :: rest) = Just (Search (unwords rest))
parseCommand _ = Nothing

parse : (input : String) -> Maybe Command
parse input = parseCommand (words input)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = 
  let store_items = items store in
    case integerToFin pos (size store) of
      Nothing => Just ("Out of range\n", store)
      Just id => Just (index id (items store) ++ "\n", store)

searchString : Nat -> (items : Vect n String) -> (str : String) -> String
searchString k [] str = ""
searchString k (x :: xs) str = 
  let rest = searchString (k + 1) xs str in
    if isInfixOf str x
      then show k ++ ":" ++ x ++ "\n" ++ rest
      else rest

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = 
  case parse input of
    Nothing => Just ("Invalid command\n", store)
    Just (Add item) => Just ("ID" ++ show (size store) ++ "\n", addToStore store item)
    Just (Get pos) => getEntry pos store
    Just Size => Just (show (size store) ++ "\n", store)
    Just (Search str) => Just (searchString 0 (items store) str, store)
    Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
