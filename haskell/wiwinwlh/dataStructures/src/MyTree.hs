{- A tree is directed graph with a single root. -}

module MyTree
  ( tree,
    postorder,
    preorder,
  )
where

-- Initialization empty O(1)
-- Size size O(1)
-- Lookup lookup O(log(n))
-- Insertion insert O(log(n))
-- Traversal traverse O(n)

import Data.Tree

{-
  A
 /  \
B   C
   / \
   D  E
-}

tree :: Tree String
tree = Node "A" [Node "B" [], Node "C" [Node "D" [], Node "E" []]]

postorder :: Tree a -> [a]
postorder (Node a ts) = elts ++ [a]
  where
    elts = concat (map postorder ts)

preorder :: Tree a -> [a]
preorder (Node a ts) = a : elts
  where
    elts = concat (map preorder ts)
