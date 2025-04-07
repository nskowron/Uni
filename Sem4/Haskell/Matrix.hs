{-
    Matrix.hs
-}

module Matrix (Matrix, addM, multM, dimsM) where

import Data.List (transpose)

type Matrix a = [[a]]

dimsM :: Matrix a -> (Int, Int)
dimsM (m:ms) = (length m, length (m:ms))

addM :: (Num a) => Matrix a -> Matrix a -> Matrix a
addM mx my = foldl (\acc (x, y) -> acc ++ [zipWith (+) x y]) [] (zip mx my)

dotprod' :: (Num a) => [a] -> [a] -> a
dotprod' xs [] = error "Non-equal length"
dotprod' [] ys = error "Non-equal length"
dotprod' xs ys = foldl (\prod (x, y) -> prod + (x * y)) 0 (zip xs ys)

multM :: (Num a) => Matrix a -> Matrix a -> Matrix a
multM mx my = foldl (\acc x -> acc ++ [map (dotprod' x) (transpose my)]) [] mx

identityM :: (Num a) => (Int, Int) -> Matrix a
identityM (m, n) = [[if i == j then 1 else 0 | j <- [1..m]] | i <- [1..n]]

powerM :: (Num a) => Matrix a -> Int -> Matrix a
powerM mx n
    | n < 0 = error "Negative exponent"
    | n == 0 = identityM (dimsM mx)
    | even n = let half = powerM mx (n `div` 2) in multM half half
    | otherwise = multM mx (powerM mx (n - 1))