{-
    Algorithm.hs
-}

module Algorithm(nextNode, nextEdge, connectSingles) where

import Graph
import Data.List
import qualified Data.Set as Set

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

-- Generates Edge based on previous two edges
nextEdge :: [Node] -> Edge -> Edge -> Edge
nextEdge allNodes (Edge f1 t1 w1) (Edge f2 t2 w2) =
    let n = if w2 < w1 - 20 then f2 else f2 + 1
        nodes = take (n + 1) allNodes -- indexing from 0
        (possible, rest) = 
            if w2 < w1 then partition (\(Node id _ _) -> id < f2 && id /= t2) nodes
            else partition (\(Node id _ _) -> id < (f2 + 1)) nodes
        (search, node) = (possible, (head . filter (\m -> nodeId m == n)) rest)
        (cn, dist) = closestNode node search
    in Edge n (nodeId cn) dist

-- Connects single nodes
connectSingles :: [Edge] -> [Node] -> [Edge]
connectSingles edges nodes = go [] (reverse edges) Set.empty nodes
    where
        go acc [] _ _ = acc
        go acc [_] _ _ = acc 
        go acc (Edge f1 t1 w1 : Edge f2 t2 w2 : es) visited nodes =
            let newVisited = Set.insert t1 (Set.insert t2 visited)
                newEdges = if f1 == f2 then es else (Edge f2 t2 w2 : es)
                newAcc = if f1 /= f2 && not (Set.member f1 visited) then
                    let (search, rest) = partition (\n -> nodeId n /= f1 && nodeId n /= t1) nodes
                        node = head (filter (\n -> nodeId n == f1) rest)
                        (cn, dist) = closestNode node search
                    in Edge f1 (nodeId cn) dist : acc
                    else acc
            in go newAcc newEdges newVisited nodes



