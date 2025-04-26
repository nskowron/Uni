{-
    Main.hs
-}
module Main where

import Graph
import Data.Aeson (encode) -- encode into json
import System.IO (stdout)
import System.Random (newStdGen, Random(randomR)) -- random seed
import qualified Data.ByteString.Lazy as BL -- writes json bytes
-- qualified - forces lib name prefix

-- writes graph as json into standard output
writeGraphJSON :: Graph -> IO ()
writeGraphJSON graph = BL.hPutStr stdout (encode graph)

-- pseudo-random Node based on previous node
nextNode :: Node -> Node
nextNode (Node id x y) = 
    let idf = fromIntegral id :: Float
        xf = fromIntegral x :: Float
        yf = fromIntegral y :: Float
        r = sqrt (xf^2 + yf^2)
        x' = ((r+10) * sin (0.3 * idf + yf))
        y' = (sqrt (abs (r^2 - x'^2)) + ((^2) . cos) (0.4 * xf + idf))
    in Node (id+1) (round x') (round y')

-- main loop
main :: IO ()
main = do
    g <- newStdGen
    let (start_x, g2) = randomR (0, 10::Int) g
    let (start_y, g3) = randomR (0, 10::Int) g2

    let nodes = Node 0 start_x start_y : map nextNode nodes

    writeGraphJSON (generateGraph (take 20 nodes) 30)