module Main

import Data.Vect
import System.REPL
import Data.Strings

infixr 5 .+.

data Schema = SString | SInt | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
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

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                              Z => Just (MkData schema _ [])
                              (S k) => Nothing

data Command : Schema -> Type where
  SetSchema : Schema -> Command schema
  Add : SchemaType schema -> Command schema
  Get : Integer -> Command schema
  Quit : Command schema

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
  where 
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs)
        = case span (/= '"') xs of
               (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
               _ => Nothing
    getQuoted _ = Nothing

parsePrefix SInt input 
  = case span isDigit input of
      ("", rest) => Nothing
      (num, rest) => Just (cast num, ltrim rest)

parsePrefix (schemal .+. schemar) input
  = case parsePrefix schemal input of
        Nothing => Nothing
        Just (l_val, input') =>
            case parsePrefix schemar input' of
                  Nothing => Nothing
                  Just (r_val, input'') => Just ((l_val, r_val), input'')

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema x = case parsePrefix schema x of
                              Nothing => Nothing
                              Just (res, "") => Just res
                              Just _ => Nothing

