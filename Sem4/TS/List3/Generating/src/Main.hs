{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-
    Main.hs
-}

import System.IO
import GHC.Generics (Generic)
import Data.Aeson (encode, ToJSON)
import qualified Data.ByteString.Lazy as BL
import System.IO (stdout)

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

writeGraphJSON :: Graph -> IO ()
writeGraphJSON graph = BL.hPutStr stdout (encode graph)

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
main = writeGraphJSON exampleGraph