{-
 Exc3.hs
-}

module Exc3 where

-- zad 10

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) | f x = x : filter' f xs
                 | otherwise = filter' f xs

take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (x:xs) | n <= 0 = []
               | otherwise = x : take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n (x:xs) | n <= 0 = x:xs
               | otherwise = drop' (n-1) xs

-- zad 12

intTest :: Bool -> Int
intTest x | x = 1
          | otherwise = 0

doubleTest :: Double -> Bool
doubleTest x = True

-- doubleTest (intTest True)
-- doubleTest (fromIntegral (intTest True))

-- helper function
divisers :: Int -> [Int]
divisers n = [x | x <- [1..n-1], mod n x == 0]

-- zad 13

phi' :: Int -> Int
phi' n = length [x | x <- [1..n], gcd x n == 1]

fifi' :: Int -> Int
fifi' n = sum (map phi' (n:divisers n))

-- zad 14

isPerfect :: Int -> Bool
isPerfect n = n == sum (divisers n)

-- zad 15

isFren :: Int -> Int -> Bool
isFren n m = n == sum (divisers m) && m == sum (divisers n)