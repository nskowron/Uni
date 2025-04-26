{-# LANGUAGE DeriveAnyClass #-} -- needed for generic
{-# LANGUAGE DeriveGeneric #-}

{-
    Graph.hs
-}

module Graph (Node(..), Edge(..), Graph(..), generateGraph) where

import Data.List (sortOn)
import GHC.Generics (Generic) -- needed for json
import Data.Aeson (ToJSON)

data Node = Node
    { id :: Int
    , x :: Int
    , y :: Int
    } deriving (Show, Generic, ToJSON)

data Edge = Edge
    { from :: Int
    , to:: Int
    , weight :: Int
    } deriving (Show, Generic, ToJSON)

data Graph = Graph
    { nodes :: [Node]
    , edges :: [Edge]
    } deriving (Show, Generic, ToJSON)

-- Euclidean distance function
distance :: Node -> Node -> Double
distance (Node _ x1 y1) (Node _ x2 y2) =
  sqrt $ fromIntegral ((x2 - x1)^2 + (y2 - y1)^2)

-- pairs of all next elements
uniquePairs :: [a] -> [(a, a)]
uniquePairs [] = []
uniquePairs (x:xs) = [(x, y) | y <- xs] ++ uniquePairs xs

-- generate edges - greedy
generateEdges :: [Node] -> Int -> [Edge]
generateEdges nodes count = map (\(Node n1 _ _, Node n2 _ _) -> Edge n1 n2 0) $
    (take count . sortOn (uncurry distance) . uniquePairs) nodes

generateGraph :: [Node] -> Int -> Graph
generateGraph nodes count = Graph nodes (generateEdges nodes count)