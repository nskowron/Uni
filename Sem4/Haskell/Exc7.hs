{-
    Exc7.hs
-}

module Exc7 where

 -- zad 59

newtype F a x = F (a, x)

instance Functor (F a) where
    fmap f (F (a, x)) = F (a, f x)