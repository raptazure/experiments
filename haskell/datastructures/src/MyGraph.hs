module MyGraph
  ( ts1,
    sc1,
    ts2,
    sc2,
    cyc3,
    ex1',
    ex2',
    x,
  )
where

{- The Graph module in the containers library is a somewhat antiquated API for working with directed graphs.
 A little bit of data wrapping makes it a little more straightforward to use.
 The library is not necessarily well­suited for large graph­ theoretic operations but is perfectly fine for example,
 to use in a typechecker which needs to resolve strongly connected components of the module definition graph. -}

import Data.Graph
import qualified Data.Graph.Inductive as G
import Data.Tree

data Grph node key = Grph
  {_graph :: Graph, _vertices :: Vertex -> (node, key, [key])}

fromList :: Ord key => [(node, key, [key])] -> Grph node key
fromList = uncurry Grph . graphFromEdges'

vertexLabels :: Functor f => Grph b t -> (f Vertex) -> f b
vertexLabels g = fmap (vertexLabel g)

vertexLabel :: Grph b t -> Vertex -> b
vertexLabel g = (\(vi, _, _) -> vi) . (_vertices g)

topo' :: Grph node key -> [node]
topo' g = vertexLabels g $ topSort (_graph g)

scc' :: Grph node key -> [[node]]
scc' g = fmap (vertexLabels g . flatten) $ scc (_graph g)

ex1 :: [(String, String, [String])]
ex1 =
  [ ("a", "a", ["b"]),
    ("b", "b", ["c"]),
    ("c", "c", ["a"])
  ]

ts1 :: [String]
ts1 = topo' (fromList ex1) -- ["a","b","c"]

sc1 :: [[String]]
sc1 = scc' (fromList ex1) -- [["a","b","c"]]

ex2 :: [(String, String, [String])]
ex2 =
  [ ("a", "a", ["b"]),
    ("b", "b", ["c"]),
    ("c", "c", ["a"]),
    ("d", "d", ["e"]),
    ("e", "e", ["f", "e"]),
    ("f", "f", ["d", "e"])
  ]

ts2 :: [String]
ts2 = topo' (fromList ex2) -- ["d","e","f","a","b","c"]

sc2 :: [[String]]
sc2 = scc' (fromList ex2) -- [["d","e","f"],["a","b","c"]]

{- The fgl library provides a more efficient graph structure and a wide variety of common graph­theoretic operations.
 For example calculating the dominance frontier of a graph shows up quite frequently in control flow analysis for compiler design.-}

cyc3 :: G.Gr Char String
cyc3 =
  G.buildGr
    [ ([("ca", 3)], 1, 'a', [("ab", 2)]),
      ([], 2, 'b', [("bc", 3)]),
      ([], 3, 'c', [])
    ]

-- Loop query
ex1' :: Bool
ex1' = G.hasLoop x

-- Dominators
ex2' :: [(G.Node, [G.Node])]
ex2' = G.dom x 0

x :: G.Gr Int ()
x = G.insEdges edges gr
  where
    gr = G.insNodes nodes G.empty
    edges = [(0, 1, ()), (0, 2, ()), (2, 1, ()), (2, 3, ())]
    nodes = zip [0, 1 ..] [2, 3, 4, 1]
