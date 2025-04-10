{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-
    Main.hs
-}

import System.IO
import GHC.Generics
import Data.Aeson (encode, ToJSON)
import qualified Data.ByteString.Lazy as BL

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

writeGraphJSON :: FilePath -> Graph -> IO ()
writeGraphJSON path graph = BL.writeFile path (encode graph)

exampleGraph :: Graph
exampleGraph = Graph
    [ Node 0 100 100
    , Node 1 300 100
    , Node 2 200 300
    ]
    [ Edge 0 1 10
    , Edge 1 2 20
    , Edge 2 0 40
    ]

main :: IO ()
main = writeGraphJSON "graph_fifo" exampleGraph