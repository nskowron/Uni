{-# LANGUAGE DeriveAnyClass #-} -- needed for generic
{-# LANGUAGE DeriveGeneric #-}

{-
    Graph.hs
-}

module Graph (Node(..), Edge(..), Graph(..), generateGraph) where

import Data.List (sortOn)
import GHC.Generics (Generic) -- needed for json
import Data.Aeson (ToJSON)
import qualified Data.Map.Strict as Map

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

-- Function to filter pairs
filterPairs :: [(Int, Int)] -> [(Int, Int)]
filterPairs = go Map.empty
    where
        go _ [] = []
        go counts ((x, y):rest)
            | count x < 4 && count y < 4 =
                let counts' = Map.insertWith (+) x 1 (Map.insertWith (+) y 1 counts)
                in (x, y) : go counts' rest
            | otherwise = go counts rest
            where
                count n = Map.findWithDefault 0 n counts


-- generate edges - greedy
generateEdges :: [Node] -> Int -> [Edge]
generateEdges nodes count = map (\(i, j) -> Edge i j 0) $
    (take count . filterPairs
    . map (\(Node i _ _, Node j _ _) -> (i,  j))
    . sortOn (uncurry distance) . uniquePairs) nodes

generateGraph :: [Node] -> Int -> Graph
generateGraph nodes count = Graph nodes (generateEdges nodes count)