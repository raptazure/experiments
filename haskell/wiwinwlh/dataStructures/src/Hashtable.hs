module Hashtable
  ( example,
  )
where

import Control.Monad.ST
import Data.HashTable.ST.Basic (HashTable, insert, lookup, new)
import Prelude hiding (lookup)

-- Hashtable parameterized by ST "thread"
type HT s = HashTable s String String

set :: ST s (HT s)
set = do
  ht <- new
  insert ht "key" "value1"
  return ht

get :: HT s -> ST s (Maybe String)
get ht = do
  val <- lookup ht "key"
  return val

example :: Maybe String
example = runST (set >>= get)

-- new :: ST s (HashTable s k v)
-- insert :: (Eq k, Hashable k) => HashTable s k v -> k -> v -> ST s ()
-- lookup :: (Eq k, Hashable k) => HashTable s k v -> k -> ST s (Maybe v)
