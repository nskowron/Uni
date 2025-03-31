{-
    Exc5.hs
-}

module Exc5 where

import Data.List (tails)

-- zad 35

{-
ghci> foldl (+) 0 big
50000005000000
(2.18 secs, 1,612,374,504 bytes)

ghci> foldr (+) 0 big
50000005000000
(1.44 secs, 1,615,375,864 bytes)

ghci> foldl1 (+) big
50000005000000
(1.97 secs, 1,612,374,400 bytes)

ghci> foldr1 (+) big
50000005000000
(1.64 secs, 1,695,375,752 bytes)
-}

-- zad 36

oldReverse :: [a] -> [a]
oldReverse [] = []
oldReverse (x:xs) = (oldReverse xs) ++ [x]

newReverse :: [a] -> [a]
newReverse = foldl (flip (:)) []

{-
oldReverse: (0.05 secs, 31,283,840 bytes)
newReverse: (0.02 secs, 3,215,168 bytes)
-}

-- zad 37

evens :: (Integral a) => [a] -> Int
evens = foldr (\x acc -> acc + (fromEnum (even x))) 0

-- zad 38

dec2Int :: [Int] -> Int
dec2Int = foldl (\acc x -> 10 * acc + x) 0

-- zad 39

{-
1. Poniewaz:
1. (((e - a1) - a2) - a3)
2. (a1 - (a2 - (a3 - e)))
-}

-- zad 40

lmss' :: [Int] -> [Int]
lmss' (x:xs) = x : longest (map lmss' (snd (descTails (tails xs))))
    where
    longest = foldr (\y arr ->
        if length y >= length arr then y
        else arr
        ) []
    descTails = foldl (\(minHead, acc) arr -> case arr of
        [] -> (minHead, acc)
        (y:ys) ->
            if y > x && y < minHead then (y, acc ++ [y:ys])
            else (minHead, acc)
        ) (maxBound, []) 