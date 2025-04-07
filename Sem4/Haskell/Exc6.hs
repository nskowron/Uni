{-
    Exc6.hs
-}

module Exc6 where

import Data.List (nub)

-- zad 45

delta' :: Int -> Char -> Int
delta' 1 '1' = 2
delta' 1 _ = 1
delta' 2 '1' = 1
delta' 2 _ = 0
delta' 0 '0' = 3
delta' 0 _ = 4
delta' 3 '1' = 1
delta' 3 _ = 4
delta' s _ = s

accept' :: Int -> Bool
accept' 2 = True
accept' _ = False

runDFA' :: (Eq s) => (s -> c -> s) -> s -> [c] -> s
runDFA' = foldl

acceptDFA' :: (Eq s) => (s -> c -> s) -> s -> (s -> Bool) -> [c] -> Bool
acceptDFA' delta s accept cs = accept (runDFA' delta s cs)

-- zad 46

convertDelta :: (Eq s) => (s -> c -> [s]) -> ([s] -> c -> [s]) 
convertDelta delta ss c  = nub (concat (map (\s -> delta s c) ss))

convertAccept :: (Eq s) => (s -> Bool) -> ([s] -> Bool)
convertAccept accept ss = or (map accept ss)

runConvertedNFA' :: (Eq s) => (s -> c -> [s]) -> s -> [c] -> [s]
runConvertedNFA' delta s cs = foldl (convertDelta delta) [s] cs

acceptConvertedNFA' :: (Eq s) => (s -> c -> [s]) -> s -> (s -> Bool) -> [c] -> Bool
acceptConvertedNFA' delta s accept cs = (convertAccept accept) (runConvertedNFA' delta s cs)

rho :: Int -> Char -> [Int]
rho 1 '1' = [1, 2]
rho _ '1' = [2]
rho _ _ = [1]

-- zad 49

dotprod' :: (Num a) => [a] -> [a] -> a
dotprod' xs [] = error "Non-equal length"
dotprod' [] ys = error "Non-equal length"
dotprod' xs ys = foldl (\prod (x, y) -> prod + (x * y)) 0 (zip xs ys)

-- zad 50

{-
    w module Matrix
-}