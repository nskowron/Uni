{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lab2 where

import Lab1
import qualified Data.Vector as V
import qualified Data.Map as Map

-- 1
-- Real multivariable polynomials
type Rx_ = Polynomial (Multiindex Integer) Double

-- Different monomial orderings
-- Lex
newtype Lex a = Lex (Multiindex a)
    deriving (Eq, Show, Num)

instance Ord a => Ord (Lex a) where
    compare (Lex (Multiindex xs)) (Lex (Multiindex ys)) = compare xs ys -- compare is lex on lists

-- lex multiindex based on a permutation (0..n-1)
-- from greatest to least
lexPerm :: [Int] -> Multiindex a -> Multiindex a
lexPerm perm (Multiindex xs) = Multiindex (map (xs !!) perm)

-- Graded Lex
newtype GradedLex a = GradedLex (Multiindex a)
    deriving (Eq, Show, Num)

instance (Ord a, Num a) => Ord (GradedLex a) where
    compare (GradedLex (Multiindex xs)) (GradedLex (Multiindex ys)) =
        let sumX = sum xs
            sumY = sum ys
        in if sumX == sumY
            then compare xs ys
            else compare sumX sumY

-- Polynomial Reduce
polynomialReduce :: (Ord a, Eq k, Num a, Num k, Fractional k) => 
    Polynomial a k -> [Polynomial a k] -> ([Polynomial a k], Polynomial a k)
polynomialReduce f gs = go f (V.replicate (length gs) 0) 0
    where
        vgs = V.fromList gs
        vltgs = V.map ltP vgs -- vector of leading terms of gs
        lt p = let (a, k) = ltP p in Polynomial (Map.singleton a k) -- syntactic sugar for leading term of p
        go 0 qs r = (V.toList qs, r)
        go p qs r = case V.findIndex (\g -> lmP g <= lmP p) vgs of
            Nothing -> 
                let ltp = Polynomial (Map.fromList [ltP p])
                in go (p - ltp) qs (r + ltp)
            Just i ->
                let ltp = Polynomial (Map.fromList [ltP p])
                    ltgi = Polynomial (Map.fromList [vltgs V.! i])
                    q = ltp / ltgi
                in go (p - q * vgs V.! i) (qs V.// [(i, qs V.! i + q)]) r
