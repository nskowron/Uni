{-
    Main.hs
-}
module Main where

import Graph
import Algorithm
import Data.Aeson (encode) -- encode into json
import System.IO (stdout, isEOF, hFlush)
import System.Random (newStdGen, Random(randomR)) -- random seed
import qualified Data.ByteString.Lazy as BL -- writes json bytes
-- qualified - forces lib name prefix


-- main loop
main :: IO ()
main = do
    g <- newStdGen
    let (start_x, g2) = randomR (0, 100::Int) g
    let (start_y, g3) = randomR (0, 100::Int) g2
    let nodes = Node 0 start_x start_y : map nextNode nodes
    let edges = Edge 1 0 0 : Edge 2 1 0 : zipWith (nextEdge nodes) edges (tail edges)

    mainLoop nodes edges 0

mainLoop :: [Node] -> [Edge] -> Int -> IO ()
mainLoop nodes edges lastC = do
    input <- getLine
    let c = read input :: Int
    let cNodes = take c nodes
    let cEdges = takeWhile (\(Edge f _ _) -> f < c) edges
    let tEdges = connectSingles cEdges cNodes

    writeGraphJSON ( -- only the update to the graph
        Graph
        (drop lastC cNodes)
        (dropWhile (\(Edge f _ _) -> f < lastC) cEdges)
        tEdges
        )

    mainLoop nodes edges c


-- writes graph as json into standard output
writeGraphJSON :: Graph -> IO ()
writeGraphJSON graph = do
    BL.hPutStr stdout (encode graph)
    putStrLn ""
    hFlush stdout