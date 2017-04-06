{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveFoldable #-}

module Control.Monad.Prob.Cons where

data Odds a = Certainly a
            | Odds a Rational (Odds a)
            deriving (Eq, Functor, Foldable, Show)
            
foldOdds :: (a -> Rational -> b -> b) -> (a -> b) -> Odds a -> b
foldOdds f b = r where
  r (Certainly x) = b x
  r (Odds x p xs) = f x p (r xs)