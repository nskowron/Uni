{-# LANGUAGE DeriveAnyClass #-} -- needed for generic
{-# LANGUAGE DeriveGeneric #-}

{-
    Graph.hs
-}

module Graph (Node(..), Edge(..), Graph(..), generateGraph) where

import Data.List (sortOn, minimumBy, nub, (\\))
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic) -- needed for json
import Data.Aeson (ToJSON)
import qualified Data.Map as Map

data Node = Node
    { id :: Int
    , x :: Int
    , y :: Int
    } deriving (Show, Eq, Generic, ToJSON)

data Edge = Edge
    { from :: Int
    , to:: Int
    , weight :: Double
    } deriving (Show, Eq, Generic, ToJSON)

data Graph = Graph
    { nodes :: [Node]
    , edges :: [Edge]
    } deriving (Show, Generic, ToJSON)

generateGraph :: [Node] -> Int -> Graph
generateGraph nodes count = Graph nodes (generateEdges nodes count)

-- Helper Functions --

newEdge :: Node -> Node -> Edge
newEdge n m = 
    Edge (nodeId n) (nodeId m) (distance n m)

nodeId :: Node -> Int
nodeId (Node id _ _) = id

-- generateEdges - another algorithm
generateEdges :: [Node] -> Int -> [Edge]
generateEdges nodes count = map (uncurry newEdge) $
    Map.foldr (++) [] $ snd $
    uncurry (ballanceFor 3 nodes nodes) $
    uncurry (ballanceFor 1 nodes (reverse nodes)) $
    connectOutwards nodes [] count Map.empty -- temp

-- connect all nodes going from the middle
connectOutwards :: [Node] -> [Node] -> Int -> Map.Map Int [(Node, Node)] -> (Int, Map.Map Int [(Node, Node)])
connectOutwards [] _ count acc = (count, acc)
connectOutwards _ _ 0 acc = (0, acc)
connectOutwards (n:ns) [] count acc = connectOutwards ns [n] count acc
connectOutwards (n:ns) visited count acc = 
    let closest = closestNode n visited
        newAcc = Map.insertWith (++) (nodeId n) [(n, closest)]
            (Map.insertWith (++) (nodeId closest) [(closest, n)] acc)
    in connectOutwards ns (n:visited) (count-1) newAcc

-- connect nodes with <= edges
ballanceFor :: Int -> [Node] -> [Node] -> Int -> Map.Map Int [(Node, Node)] -> (Int, Map.Map Int [(Node, Node)])
ballanceFor _ _ [] count acc = (count, acc)
ballanceFor _ _ _ 0 acc = (0, acc)
ballanceFor edges nodes (n:ns) count acc =
    let connected = connectedNodes n acc
    in  if length connected > edges
        then ballanceFor edges nodes ns count acc
        else
            let rest = nodes \\ (n:connected)
                closestWith = closestNodeWith (edges + 1) n rest acc
                closest = fromMaybe (closestNode n rest) closestWith
                newAcc = Map.insertWith (++) (nodeId n) [(n, closest)]
                    (Map.insertWith (++) (nodeId closest) [(closest, n)] acc)
            in ballanceFor edges nodes ns (count-1) newAcc

-- get connected nodes
connectedNodes :: Node -> Map.Map Int [(Node, Node)] -> [Node]
connectedNodes node nodes = 
    let pairs = Map.findWithDefault [] (nodeId node) nodes
    in map snd pairs

closestNodeWith :: Int -> Node -> [Node] -> Map.Map Int [(Node, Node)] -> Maybe Node
closestNodeWith edges node nodes pairs = foldl (\n m ->
    if length (Map.findWithDefault [] (nodeId m) pairs) > edges then n
    else if (distance node m) > 100 then n
    else if n == Nothing then Just m
    else if (distance node (fromMaybe (Node 0 0 0) n)) > (distance node m) then Just m
    else n
    ) Nothing nodes

-- Closest node by euclidean
closestNode :: Node -> [Node] -> Node
closestNode node = minimumBy (\n1 n2 -> compare (distance node n1) (distance node n2))

-- Euclidean distance function
distance :: Node -> Node -> Double
distance (Node _ x1 y1) (Node _ x2 y2) =
  sqrt $ fromIntegral ((x2 - x1)^2 + (y2 - y1)^2)