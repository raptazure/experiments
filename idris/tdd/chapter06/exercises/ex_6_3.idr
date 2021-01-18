module Main

import Data.Vect
import System.REPL
import Data.Strings
import Data.List

infixr 5 .+.

data Schema = SString | SInt | SChar | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
  constructor MkData
  schema : Schema
  size : Nat
  items : Vect size (SchemaType schema)

addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size store) newitem = MkData schema _ (addToData store)
  where
    addToData : Vect oldsize (SchemaType schema)  -> Vect (S oldsize) (SchemaType schema)
    addToData [] = [newitem]
    addToData (x :: xs) = x :: addToData xs

display : {schema : _} -> SchemaType schema -> String
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = SChar} item = show item
display {schema = (x .+. y)} (iteml, itemr) 
  = display iteml ++ ", " ++ display itemr

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
  case integerToFin pos (size store) of
    Nothing => Just ("Out of range\n", store)
    Just id => Just (display (index id store_items) ++ "\n", store)

data Command : Schema -> Type where
  SetSchema : Schema -> Command schema
  Add : SchemaType schema -> Command schema
  Get : Maybe Integer -> Command schema
  Quit : Command schema

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
  where getQuoted : List Char -> Maybe (String, String)
        getQuoted ('"' :: xs) = case Data.List.span (/= '"') xs of
            (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
            _ => Nothing
        getQuoted _ = Nothing
parsePrefix SInt input = 
  case span isDigit input of
    ("", rest) => Nothing
    (num, rest) => Just (cast num, ltrim rest)
parsePrefix SChar input =
  case unpack input of
    (c :: cs) => Just (c, ltrim (pack cs))
    [] => Nothing
parsePrefix (schemal .+. schemar) input = do 
  (l_val, input') <- parsePrefix schemal input
  (r_val, input'') <- parsePrefix schemar input'
  Just ((l_val, r_val), input'')
  -- case parsePrefix schemal input of
  --   Nothing => Nothing
  --   Just (l_val, input') => case parsePrefix schemar input' of
  --     Nothing => Nothing
  --     Just (r_val, input'') => Just ((l_val, r_val), input'')

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = 
  case parsePrefix schema input of
    Just (res, "") => Just res
    Just _ => Nothing
    Nothing => Nothing

parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs) = case xs of
  [] => Just SString
  _ => case parseSchema xs of
    Nothing => Nothing
    Just xs_sch => Just (SString .+. xs_sch)
parseSchema ("Char" :: xs) = case xs of
  [] => Just SChar
  _ => case parseSchema xs of
    Nothing => Nothing
    Just xs_sch => Just (SChar .+. xs_sch)
parseSchema ("Int" :: xs) = case xs of
  [] => Just SInt
  _ => case parseSchema xs of
    Nothing => Nothing
    Just xs_sch => Just (SInt .+. xs_sch)
parseSchema _ = Nothing

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = 
  case size store of
    Z => Just (MkData schema _ [])
    (S k) => Nothing

parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "add" rest = 
  case parseBySchema schema rest of
    Nothing => Nothing
    Just restok => Just (Add restok)
parseCommand schema "get" "" = Just (Get Nothing)
parseCommand schema "get" val = 
  case all isDigit (unpack val) of
    False => Nothing
    True => Just (Get (Just (cast val)))
parseCommand schema "quit" "" = Just Quit
parseCommand schema "schema" rest = 
  -- do schemaok <- parseSchema (words rest)
  --    Just (SetSchema schemaok)
  case parseSchema (words rest) of
    Nothing => Nothing
    Just schemaok => Just (SetSchema schemaok)
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
  (cmd, args) => parseCommand schema cmd (ltrim args)

showAll : {schema : _} -> Nat -> Vect size (SchemaType schema) -> String
showAll k [] = ""
showAll k (x :: xs) = show k ++ ": " ++ display x ++ "\n" 
                      ++ showAll (k + 1) xs

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse (schema store) input of
  Nothing => Just ("Invalid command\n", store)  
  Just (Add item) => 
    Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
  Just (SetSchema schema') => case setSchema store schema' of 
    Nothing => Just ("Cannot update schema\n", store)
    Just store' => Just ("Ok\n", store')
  Just (Get (Just pos)) => getEntry pos store
  Just (Get Nothing) => Just (showAll 0 (items store) ++ "\n", store)
  Just Quit => Nothing

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput
