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
lexPerm :: [Int] -> Multiindex a -> Lex a
lexPerm perm (Multiindex xs) = Lex $ Multiindex (map (xs !!) perm)

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

gradedLexPerm :: [Int] -> Multiindex a -> GradedLex a
gradedLexPerm perm (Multiindex xs) = GradedLex $ Multiindex (map (xs !!) perm)

-- Polynomial Reduce
polynomialReduce :: (Ord a, Eq k, Num a, Num k, Fractional k) => 
    Polynomial a k -> [Polynomial a k] -> ([Polynomial a k], Polynomial a k)
polynomialReduce f gs = go f (V.replicate (length gs) 0) 0
    where
        vgs = V.fromList gs
        vltgs = V.map ltP vgs -- vector of leading terms of gs
        go 0 qs r = (V.toList qs, r)
        go p qs r = 
            let ltp = Polynomial (Map.fromList [ltP p])
            in case V.findIndex (\g -> snd (divP ltp (Polynomial (Map.fromList [ltP g]))) == 0) vgs of
                Nothing -> go (p - ltp) qs (r + ltp)
                Just i ->
                    let ltgi = Polynomial (Map.fromList [vltgs V.! i])
                        q = fst $ divP ltp ltgi
                    in go (p - q * (vgs V.! i)) (qs V.// [(i, (qs V.! i) + q)]) r
