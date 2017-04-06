{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies     #-}


module Control.Monad.Prob.Map where
  
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Semiring

import Control.Monad.Constrained

import Control.Monad.Prob.List

newtype Prob s a
  = Prob
  { runProb :: Map a s }
  
reify :: (Num s, Ord a) => Dist s a -> Map a s
reify (Dist xs) = Map.fromListWith (+) xs

reflect :: Map a s -> Dist s a
reflect xs = Dist (Map.toList xs)
  
instance (Semiring s, Ord a) => Monoid (Prob s a) where
  mempty = Prob Map.empty
  mappend (Prob xs) (Prob ys)
    = Prob (Map.unionWith (<+>) xs ys)
    
instance Semiring s => Functor (Prob s) where
  type Suitable (Prob s) a = Ord a
  fmap f (Prob xs) = Prob (Map.mapKeysWith (<+>) f xs)
  
instance Semiring s => Applicative (Prob s) where
  lower = reify . lowerP . hoist reflect
  
scaled
    :: Semiring s => Prob s a -> s -> Prob s a
scaled (Prob xs) x = Prob (fmap (x <.>) xs)
  
instance Semiring s => Monad (Prob s) where
     Prob xs >>= f = Map.foldMapWithKey (scaled . f) xs
