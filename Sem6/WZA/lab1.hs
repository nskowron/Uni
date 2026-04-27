module Lab1 where

import qualified Data.Map as Map

-- 1
-- Complex Numbers
data Complex a = Complex a a deriving (Eq)

instance (Show a) => Show (Complex a) where
    show (Complex a b) = show a ++ " + " ++ show b ++ "i"

instance (Num a) => Num (Complex a) where
    (Complex a b) + (Complex c d) = Complex (a + c) (b + d)
    (Complex a b) * (Complex c d) = Complex (a * c - b * d) (a * d + b * c)
    negate (Complex a b) = Complex (negate a) (negate b)
    fromInteger n = Complex (fromInteger n) 0

instance (Num a, Fractional a) => Fractional (Complex a) where
    (Complex a b) / (Complex c d) = 
        let denom = c * c + d * d
        in Complex ((a * c + b * d) / denom) ((b * c - a * d) / denom) 
    fromRational r = Complex (fromRational r) 0

-- Gaussian Ring
type Gaussian = Complex Integer

fromGaussian :: (Num a) => Gaussian -> Complex a
fromGaussian (Complex a b) = Complex (fromInteger a) (fromInteger b)

-- norm
normG :: Gaussian -> Double
normG (Complex a b) = sqrt $ 
    fromInteger a * fromInteger a + fromInteger b * fromInteger b

normC :: Complex Double -> Double
normC (Complex a b) = sqrt $ a * a + b * b

-- division with remainder
roundC :: (RealFrac a) => Complex a -> Gaussian
roundC (Complex a b) = Complex (round a) (round b)

divG :: Gaussian -> Gaussian -> (Gaussian, Gaussian)
divG _ (Complex 0 0) = error "Division by zero"
divG a b =
    let q = roundC (fromGaussian a / fromGaussian b)
        r = a - q * b
    in (q, r)

-- GCD
gcdG :: Gaussian -> Gaussian -> Gaussian
gcdG a (Complex 0 0) = a
gcdG a b =
    let (_, r) = divG a b
    in gcdG b r

gcdListG :: [Gaussian] -> Gaussian
gcdListG = foldl1 gcdG

-- LCM
lcmG :: Gaussian -> Gaussian -> Gaussian
lcmG a b = 
    let (q, _) = divG (a * b) (gcdG a b)
    in q

lcmListG :: [Gaussian] -> Gaussian
lcmListG = foldl1 lcmG

-- 2
-- Polynomials
-- a - index type
-- k - coefficients type
newtype Polynomial a k = Polynomial (Map.Map a k)

instance (Ord a, Eq k, Num a, Num k) => Num (Polynomial a k) where
    (Polynomial xs) + (Polynomial ys) = Polynomial $ Map.filter (/= 0) $ Map.unionWith (+) xs ys
    (Polynomial xs) * (Polynomial ys) = Polynomial $ Map.filter (/= 0) $ Map.unionsWith (+)
        [Map.singleton (ax + ay) (kx * ky) | (ax, kx) <- Map.assocs xs, (ay, ky) <- Map.assocs ys]
    negate (Polynomial xs) = Polynomial (Map.map negate xs)
    fromInteger n = Polynomial (Map.singleton 0 (fromInteger n))

instance (Ord a, Eq k, Num a, Num k) => Eq (Polynomial a k) where
    xs == ys = ltP (xs - ys) == (0, 0)

-- Real Polynomials
type Rx = Polynomial Integer Double

instance Show Rx where
    show (Polynomial xs) = drop 3 $ Map.foldlWithKey (\acc a x -> acc ++ " + " ++ show x ++ "x^" ++ show a) "" xs

-- leading stuff
ltP :: (Ord a, Eq k, Num a, Num k) => Polynomial a k -> (a, k)
ltP (Polynomial xs) = 
    if Map.null xs
    then ltP 0
    else Map.findMax xs

lmP :: (Ord a, Eq k, Num a, Num k) => Polynomial a k -> a
lmP = fst . ltP

lcP :: (Ord a, Eq k, Num a, Num k) => Polynomial a k -> k
lcP = snd . ltP

-- norm
normRx :: Rx -> Integer
normRx = lmP

-- division with remainder
divP :: (Ord a, Eq k, Num a, Num k, Fractional k) => Polynomial a k -> Polynomial a k -> (Polynomial a k, Polynomial a k)
divP _ 0 = error "Division by zero"
divP xs ys =
    let go acc xs ys =
            let (ax, kx) = ltP xs
                (ay, ky) = ltP ys
                q = Polynomial (Map.singleton (ax - ay) (kx / ky))
            in if ay <= ax && q /= 0
                then go (acc + q) (xs - q * ys) ys
                else acc
        q = go 0 xs ys
        r = xs - q * ys
    in (q, r)

-- GCD
gcdP :: (Ord a, Eq k, Num a, Num k, Fractional k) => Polynomial a k -> Polynomial a k -> Polynomial a k
gcdP a 0 = a
gcdP a b =
    let (_, r) = divP a b
    in gcdP b r

egcdP :: (Ord a, Eq k, Num a, Num k, Fractional k) => 
    Polynomial a k -> Polynomial a k -> (Polynomial a k, Polynomial a k, Polynomial a k)
egcdP a 0 = (a, 1, 0)
egcdP a b =
    let (q, r) = divP a b
        (g, s, t) = egcdP b r
    in (g, t, s - q * t)

-- LCM
lcmP :: (Ord a, Eq k, Num a, Num k, Fractional k) => Polynomial a k -> Polynomial a k -> Polynomial a k
lcmP a b = 
    let (q, _) = divP (a * b) (gcdP a b)
    in q

-- 3
-- Multiindices
newtype Multiindex a = Multiindex [a]
    deriving (Eq, Show)

instance (Ord a) => Ord (Multiindex a) where
    (Multiindex xs) <= (Multiindex ys) = 
        foldl (\a (x, y) -> a && x <= y) True (zip xs ys)
    (Multiindex xs) >= (Multiindex ys) = 
        foldl (\a (x, y) -> a && x >= y) True (zip xs ys)

instance (Num a) => Num (Multiindex a) where
    (Multiindex xs) + (Multiindex ys) = Multiindex (zipWith (+) xs ys)
    (Multiindex xs) * (Multiindex ys) = Multiindex (zipWith (*) xs ys) -- todo
    negate (Multiindex xs) = Multiindex (map negate xs)
    fromInteger n = Multiindex $ repeat (fromInteger n)

-- <= - minimal elements from a list of multiindices
minMI :: (Ord a) => [Multiindex a] -> [Multiindex a]
minMI xs = go [] xs where
    go acc [] = acc
    go acc (y:ys) = 
        if any (\x -> x <= y && x /= y) xs 
        then go acc ys
        else go (y:acc) ys

-- norm
normMI :: (Num a) => Multiindex a -> a
normMI (Multiindex xs) = sum xs
