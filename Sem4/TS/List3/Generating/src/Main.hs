{-
    Main.hs
-}
module Main where

import Graph
import Data.Aeson (encode) -- encode into json
import System.IO (stdout, isEOF, hFlush)
import System.Random (newStdGen, Random(randomR)) -- random seed
import qualified Data.ByteString.Lazy as BL -- writes json bytes
-- qualified - forces lib name prefix

-- writes graph as json into standard output
writeGraphJSON :: Graph -> IO ()
writeGraphJSON graph = do
    BL.hPutStr stdout (encode graph)
    putStrLn ""
    hFlush stdout

-- pseudo-random Node based on previous node
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

-- main loop
main :: IO ()
main = do
    g <- newStdGen
    let (start_x, g2) = randomR (0, 100::Int) g
    let (start_y, g3) = randomR (0, 100::Int) g2
    let nodes = Node 0 start_x start_y : map nextNode nodes

    -- writeGraphJSON (generateGraph (take 20 nodes) 30)
    mainLoop nodes

mainLoop :: [Node] -> IO ()
mainLoop nodes = do
    input <- getLine
    let c = read input :: Int

    writeGraphJSON (generateGraph (take c nodes) (round (fromIntegral c * 3 / 2)))
    mainLoop nodes