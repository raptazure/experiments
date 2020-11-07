module Main where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Writer
import Data.DList as DL
import Data.Sequence as S
import Data.Tree
import Hashtable as H
import MyGraph
import MyMap
import MySet
import MyTree
import MyVector as V
import Unordered as U

main :: IO ()
main = do
  mapPrint
  treePrint
  setPrint
  vectorPrint
  unorderedPrint
  print H.example
  graphPrint
  print logger
  print a
  print a0
  print a1

mapPrint :: IO ()
mapPrint = do
  print $ lkup 1 "not found"
  print $ lkup 3 "not found"

treePrint :: IO ()
treePrint = do
  print $ drawTree tree
  print $ drawForest (subForest tree)
  print $ flatten tree
  print $ levels tree
  print $ preorder tree
  print $ postorder tree

setPrint :: IO ()
setPrint = do
  -- print set
  print $ memtest 4

vectorPrint :: IO ()
vectorPrint = do
  print $ V.example1
  print $ norm vec
  vecIO >>= print
  print $ runST vecST

unorderedPrint :: IO ()
unorderedPrint = do
  print U.example1
  print U.example2

graphPrint :: IO ()
graphPrint = do
  print ts1
  print sc1
  print ts2
  print sc2
  print cyc3
  print ex1'
  print ex2'
  print x

logger :: Writer (DList Int) ()
logger = replicateM_ 10 $ tell (DL.singleton 0)

a :: Seq Int
a = S.fromList [1, 2, 3]

a0 :: Seq Int
a0 = a |> 4

a1 :: Seq Int
a1 = 0 <| a