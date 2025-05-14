{-
    Algorithm.hs
-}

module Algorithm(nextNode, nextEdge, nextWebbing, connectSingles) where

import Graph
import Data.List
import qualified Data.Map as Map
--import qualified Data.Set as Set

-- Closest node by euclidean
closestNode :: Node -> [Node] -> (Node, Double)
closestNode node nodes = 
    let cn = minimumBy (\n1 n2 -> compare (distance node n1) (distance node n2)) nodes
    in (cn, distance node cn)

-- Pseudo-random Node based on previous node
nextNode :: Node -> Node
nextNode (Node id x y) = 
    let idf = fromIntegral id :: Float
        xf = fromIntegral x :: Float
        yf = fromIntegral y :: Float
        r = sqrt (xf^2 + yf^2)
        x' = ((r+20) * sin (0.3 * idf + yf))
        y' = (if even id then 1 else -1)
            * (sqrt (abs (r^2 - x'^2)) + 10 * ((^2) . cos) (0.4 * xf + idf))
    in Node (id+1) (round x') (round y')

-- Edge from node id to closest previous
nextEdge :: [Node] -> Int -> Edge
nextEdge nodes id =
    let ([node], rest) = partition (\n -> nodeId n == id) (take (id + 1) nodes) -- node and all closer to 0
        (cn, dist) = closestNode node rest
    in Edge id (nodeId cn) dist

-- kth connection around the graph - like spider web
nextWebbing :: [Node] -> [Edge] -> Int -> [Edge]
nextWebbing nodes edges k =
    let count = 4 * (k ^ 2) -- how many nodes we want webbed
        len = floor $ 3 * fromIntegral k * sqrt (fromIntegral k :: Float) -- length of the web (duh)
        nc = drop (count - len) $ take count nodes -- furthest len nodes
        ec = drop (count - 1 - len) $ take (count - 1) edges -- furthest len edges
        (m:ms) = sortBy (\(Node _ x1 y1) (Node _ x2 y2) -> 
            compare (atan2 (fromIntegral x1) (fromIntegral y1)) (atan2 (fromIntegral x2) (fromIntegral y2))) nc -- sort nodes by angle - in a circle
    in fst $ foldr (\o (acc, n) -> 
        let e = Edge (max (nodeId o) (nodeId n)) (min (nodeId o) (nodeId n)) (distance o n)
        in if e `notElem` ec
            then (e:acc, o)
            else (acc, o)
    ) ([], m) (m:ms)

-- Connects single nodes to finalize the graph
connectSingles :: [Edge] -> [Node] -> [Edge]
connectSingles edges nodes =
    let map = foldl (\m e -> Map.insertWith (++) (from e) [e] m) Map.empty edges
        go acc [] = acc
        go acc (n:ns) = case Map.findWithDefault [] (nodeId n) map of
            [Edge f t w] -> 
                let (cut, rest) = partition (\m -> nodeId m `elem` [f, t]) nodes
                    node = head $ filter (\c -> nodeId c == f) cut
                    (cn, dist) = closestNode node rest
                in go (Edge f (nodeId cn) dist:acc) ns
            _ -> go acc ns
    in go [] nodes
