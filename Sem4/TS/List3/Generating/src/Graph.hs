{-# LANGUAGE DeriveAnyClass #-} -- needed for generic
{-# LANGUAGE DeriveGeneric #-}

{-
    Graph.hs
-}

module Graph (Node(..), Edge(..), Graph(..), nodeId, distance) where

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
    , tempEdges :: [Edge]
    } deriving (Show, Generic, ToJSON)


-- Helper Functions --

nodeId :: Node -> Int
nodeId (Node id _ _) = id

-- Euclidean distance function
distance :: Node -> Node -> Double
distance (Node _ x1 y1) (Node _ x2 y2) =
  sqrt $ fromIntegral ((x2 - x1)^2 + (y2 - y1)^2)