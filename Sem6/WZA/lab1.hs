module Lab1 where

-- utilities
zipDef :: a -> (a -> a -> a) -> [a] -> [a] -> [a]
zipDef def f xs ys = go xs ys
  where    
    go [] [] = []
    go [] (y:ys) = f def y : go [] ys
    go (x:xs) [] = f x def : go xs []
    go (x:xs) (y:ys) = f x y : go xs ys

-- 1
-- Complex Numbers
data Complex a = Complex
    { real :: a
    , imag :: a
    } deriving (Eq, Show)

instance (Num a) => Num (Complex a) where
    (Complex a b) + (Complex c d) = Complex (a + c) (b + d)
    (Complex a b) * (Complex c d) = Complex (a * c - b * d) (a * b + c * d)
    negate (Complex a b) = Complex (negate a) (negate b)
    abs _ = error "abs is not defined for Complex numbers"
    signum _ = error "signum is not defined for Complex numbers"
    fromInteger n = Complex (fromInteger n) 0

instance (Fractional a) => Fractional (Complex a) where
    recip (Complex a b) =
        let denom = a * a + b * b
        in Complex (a / denom) (-b / denom)
    fromRational r = Complex (fromRational r) 0

-- Gaussian Ring
type Gaussian = Complex Integer

fromGaussian :: (Num a) => Gaussian -> Complex a
fromGaussian (Complex a b) = Complex (fromInteger a) (fromInteger b)

-- 1.a
-- norm
normC :: Gaussian -> Double
normC (Complex a b) = sqrt $ 
    fromInteger a * fromInteger a + fromInteger b * fromInteger b

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
lcmG a b = fst $ divG (a * b) (gcdG a b)

lcmListG :: [Gaussian] -> Gaussian
lcmListG = foldl1 lcmG

-- 2
-- Polynomials
newtype Polynomial a = Polynomial [a] deriving (Show)

instance Functor Polynomial where
    fmap f (Polynomial xs) = Polynomial (map f xs)

instance (Eq a, Num a) => Eq (Polynomial a) where
    (Polynomial []) == (Polynomial []) = True
    (Polynomial (x:xs)) == (Polynomial []) = x == 0 && Polynomial xs == Polynomial []
    (Polynomial []) == (Polynomial (y:ys)) = y == 0 && Polynomial ys == Polynomial []
    (Polynomial (x:xs)) == (Polynomial (y:ys)) = x == y && Polynomial xs == Polynomial ys

instance (Num a) => Num (Polynomial a) where
    (Polynomial xs) + (Polynomial ys) = Polynomial (zipDef 0 (+) xs ys)
    (Polynomial xs) * (Polynomial ys) = sum [Polynomial (replicate i 0 ++ map (* x) ys) | (x, i) <- zip xs [0..]]
    negate (Polynomial xs) = Polynomial (map negate xs)
    abs _ = error "abs is not defined for Polynomials"
    signum _ = error "signum is not defined for Polynomials"
    fromInteger n = Polynomial [fromInteger n]

-- Real Polynomials
type Rx = Polynomial Double

-- 2.a

-- 3
-- Multiindices
newtype Multiindex a = Multiindex [a]
    deriving (Eq, Show)

instance (Ord a) => Ord (Multiindex a) where
    (Multiindex xs) <= (Multiindex ys) = 
        foldl (\a (x, y) -> a && x <= y) True (zip xs ys)
    (Multiindex xs) >= (Multiindex ys) = 
        foldl (\a (x, y) -> a && x >= y) True (zip xs ys)

-- <= - minimal elements from a list of multiindices
minMI :: (Ord a) => [Multiindex a] -> [Multiindex a]
minMI xs = go [] xs where
    go acc [] = acc
    go acc (y:ys) = if any (\x -> x <= y && x /= y) xs then go acc ys else go (y:acc) ys
