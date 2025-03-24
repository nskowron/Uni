{-
 Exc4.hs
-}

module Exc4 where

-- zad 20

splits' :: [a] -> [([a], [a])]
splits' [] = [([], [])]
splits' (x:xs) = ([], x:xs) : (map (\(l, r) -> (x:l, r)) (splits' xs))

-- zad 21

partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' _ [] = ([], [])
partition' f (x:xs) | f x       = (x:l, r)
                    | otherwise = (l, x:r)
                    where (l, r) = partition' f xs

-- zad 22

insertions' :: a -> [a] -> [[a]]
insertions' x [] = [[x]]
insertions' x (y:ys) = (x:y:ys) : (map (y:) (insertions' x ys))

mapAndConcat' :: (a -> [a]) -> [a] -> [a]
mapAndConcat' _ [] = []
mapAndConcat' f (x:xs) = (f x) ++ (mapAndConcat' f xs)

permutations' :: [a] -> [[a]]
permutations' [] = [[]]
permutations' (x:xs) = mapAndConcat' (insertions' x) (permutations' xs)

-- zad 23

hitsRec :: Int -> Int -> [Int] -> Bool
hitsRec _ _ [] = False
hitsRec x s (y:ys) | x + s == y = True
                   | x - s == y = True
                   | otherwise  = hitsRec x (s+1) ys || hitsRec y 1 ys
                   -- x == y for checking if they hit in straight lines, but we don't need it here

hits :: [Int] -> Bool
hits [] = False
hits (x:xs) = hitsRec x 1 xs

queens :: Int -> [[Int]]
queens n | n < 1     = [[]]
         | otherwise = filter (not.hits) (permutations' [1..n])

-- zad 26

isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (y:x:xs) = y <= x && isSorted (x:xs)

-- zad 27

bubble :: (Ord a) => [a] -> [a]
bubble [] = []
bubble [x] = [x]
bubble (y:x:xs) | y <= x        = y : bubble (x:xs)
                | otherwise     = x : bubble (y:xs)

bubbleSort' :: (Ord a) => [a] -> [a]
bubbleSort' [] = []
bubbleSort' [x] = [x]
bubbleSort' xs | isSorted xs    = xs
               | otherwise      = bubbleSort' (bubble xs)

-- zad 28

insert' :: (Ord a) => a -> [a] -> [a]
insert' x [] = [x]
insert' x (y:ys) | x > y        = y : insert' x ys
                 | otherwise    = x:y:ys

insertionSort' :: (Ord a) => [a] -> [a]
insertionSort' [] = []
insertionSort' (x:xs) = insert' x (insertionSort' xs)

-- zad 30

filter'' :: (a -> Bool) -> [a] -> [a]
-- concat :: [[a]] -> [a]
-- map :: (a -> b) -> [a] -> [b]
-- box :: a -> [a]

filter'' p = concat . map box
    where box x = if p x then [x] else []

-- zad 31

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x           = x : takeWhile' p xs
                    | otherwise     = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) | p x           = takeWhile' p xs
                    | otherwise     = x:xs