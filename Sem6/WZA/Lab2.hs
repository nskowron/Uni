{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- deriving Num

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
    deriving (Eq, Num)

instance (Show a) => Show (Lex a) where
    show (Lex mi) = show mi

instance Ord a => Ord (Lex a) where
    compare (Lex (Multiindex xs)) (Lex (Multiindex ys)) = compare xs ys -- compare is lex on lists

-- lex multiindex based on a permutation (0..n-1)
-- from greatest to least
miPerm :: [Int] -> Multiindex a -> Multiindex a
miPerm perm (Multiindex xs) = Multiindex (map (xs !!) perm)

fromMiPerm :: (Num a) => [Int] -> Multiindex a -> Multiindex a
fromMiPerm perm (Multiindex xs) = 
    Multiindex $ V.toList $ V.replicate (length xs) 0 V.// zip perm xs

-- helpers, temporary
mapExponents :: (Ord b) => (a -> b) -> Polynomial a k -> Polynomial b k
mapExponents f (Polynomial xs) = Polynomial (Map.mapKeys f xs)

miPermLex :: [Int] -> Lex a -> Lex a
miPermLex perm (Lex mi) = Lex (miPerm perm mi)

fromMiPermLex :: (Num a) => [Int] -> Lex a -> Lex a
fromMiPermLex perm (Lex mi) = Lex (fromMiPerm perm mi)

-- Graded Lex
newtype GradedLex a = GradedLex (Multiindex a)
    deriving (Eq, Num)

instance (Show a) => Show (GradedLex a) where
    show (GradedLex mi) = show mi

instance (Ord a, Num a) => Ord (GradedLex a) where
    compare (GradedLex (Multiindex xs)) (GradedLex (Multiindex ys)) =
        let diff = sum $ zipWith (-) xs ys
        in if diff == 0
            then compare xs ys
            else compare diff 0

-- Polynomial Reduce
polynomialReduce :: (Ord a, Eq k, Num a, Num k, Fractional k) => 
    Polynomial a k -> [Polynomial a k] -> ([Polynomial a k], Polynomial a k)
polynomialReduce f gs = go f (V.replicate (length gs) 0) 0
    where
        vgs = V.fromList gs
        vltgs = V.map (\g -> Polynomial $ Map.fromList [ltP g]) vgs -- vector of leading terms of gs
        go 0 qs r = (V.toList qs, r)
        go p qs r = 
            let ltp = Polynomial (Map.fromList [ltP p])
            in case V.findIndex (\ltg -> snd (divP ltp ltg) == 0) vltgs of
                Nothing -> go (p - ltp) qs (r + ltp)
                Just i ->
                    let q = fst $ divP ltp (vltgs V.! i)
                    in go (p - q * (vgs V.! i)) (qs V.// [(i, (qs V.! i) + q)]) r
