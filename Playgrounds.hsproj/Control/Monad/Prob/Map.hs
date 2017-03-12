{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies     #-}


module Control.Monad.Prob.Map where
  
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Semiring

import Control.Monad.Constrained

import Control.Monad.Prob.List

newtype Weighted s a
  = Weighted
  { runWeighted :: Map a s }
  
reify :: (Semiring s, Ord a) => Dist s a -> Weighted s a
reify (Dist xs) = Weighted (Map.fromListWith (<+>) xs)

reflect :: Weighted s a -> Dist s a
reflect (Weighted xs) = Dist (Map.toList xs)
  
instance (Semiring s, Ord a) => Monoid (Weighted s a) where
  mempty = Weighted Map.empty
  mappend (Weighted xs) (Weighted ys)
    = Weighted (Map.unionWith (<+>) xs ys)
    
instance Semiring s => Functor (Weighted s) where
  type Suitable (Weighted s) a = Ord a
  fmap f (Weighted xs) = Weighted (Map.mapKeysWith (<+>) f xs)
  
instance Semiring s => Applicative (Weighted s) where
  lower = reify . lowerP . hoist reflect
  
scaled
    :: Semiring s => Weighted s a -> s -> Weighted s a
scaled (Weighted xs) x = Weighted (fmap (x <.>) xs)

  
instance Semiring s => Monad (Weighted s) where
     Weighted xs >>= f = Map.foldMapWithKey (scaled . f) xs
