module Lab3 where

import Lab1
import Lab2
import Data.List (nubBy)
import qualified Data.Vector as V
import qualified Data.Map as Map


-- gcd and lcd for multiindices
-- necessary because of the difference in calculation
-- TODO: fix it with proper polymorphism (i cant watch)
gcdM :: (Num a) => a -> a -> a
gcdM x y = 
    let diff = signum (x - y)
        y_min = signum (diff + 1)
        x_min = 1 - y_min
    in x * x_min + y * y_min

lcmM :: (Num a) => a -> a -> a
lcmM x y = x + y - gcdM x y

-- syzygium
syzygium :: (Ord a, Eq k, Num a, Num k, Fractional k) 
    => Polynomial a k -> Polynomial a k -> Polynomial a k
syzygium f g =
    let x = Polynomial $ Map.singleton (lcmM (lmP f) (lmP g)) 1
        ltf = Polynomial $ Map.fromList [ltP f]
        ltg = Polynomial $ Map.fromList [ltP g]
    in f * fst (divP x ltf) - g * fst (divP x ltg)

-- Buchberger algorithm
-- would be more efficient with Set instead of List
-- but problem is how to define Ord on Polynomials
buchberger :: (Ord a, Eq k, Num a, Num k, Fractional k) 
    => [Polynomial a k] -> [Polynomial a k]
buchberger c = 
    let g = nubBy (\f g -> snd (divP f g) == 0) $ 
            c ++ 
            filter (/= 0) 
            [snd $ polynomialReduce (syzygium f g) c
            | f <- c, g <- c
            ]
    in if length g == length c
        then g
        else buchberger g