module Lab3 where

import Lab1
import Lab2
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Set as Set


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
buchberger :: (Ord a, Ord k, Eq k, Num a, Num k, Fractional k) 
    => [Polynomial a k] -> [Polynomial a k]
buchberger g = go (Set.fromList g)
    where
        go c = 
            let lc = Set.elems c
                g = Set.delete 0 $ Set.union c $ Set.fromList $ 
                    [snd $ polynomialReduce (syzygium f g) lc
                    | f <- lc, g <- lc]
            in if c == g
                then lc
                else go g