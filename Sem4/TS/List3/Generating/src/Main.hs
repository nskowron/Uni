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
    let edges = map (nextEdge nodes) [1..]
    let webs = [] : map (nextWebbing nodes edges) [2..]
    mainLoop nodes edges webs 0

mainLoop :: [Node] -> [Edge] -> [[Edge]] -> Int -> IO ()
mainLoop nodes edges webs lastC = do
    input <- getLine
    let c = read input :: Int
    let webIndex x = floor (sqrt (fromIntegral x / 4.0))
    let cNodes = drop lastC $ take c nodes
    let cEdges = drop (lastC - 1) (take (c - 1) edges) ++ concat (drop (webIndex lastC) (take (webIndex c) webs))
    let tEdges = connectSingles cEdges cNodes

    writeGraphJSON (Graph cNodes cEdges tEdges)
    mainLoop nodes edges webs c


-- writes graph as json into standard output
writeGraphJSON :: Graph -> IO ()
writeGraphJSON graph = do
    BL.hPutStr stdout (encode graph)
    putStrLn ""
    hFlush stdout