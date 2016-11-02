{-# LANGUAGE TupleSections, PatternSynonyms, ViewPatterns, DeriveFunctor, DeriveFoldable #-}

module Control.Monad.Prob where

import Data.Semiring
import Control.Applicative
import Text.Parse
import Test.QuickCheck
import Prelude.Extras
import Control.Applicative.Alternative

newtype WeightedT s m a = WeightedT
  { runWeightedT :: m (a, s) 
  } deriving Functor
  
instance (Semiring s, Applicative m) => Applicative (WeightedT s m) where
  pure x = WeightedT (pure (x,one))
  fs <*> xs = WeightedT $
    liftA2 k (runWeightedT fs) (runWeightedT xs)
      where k ~(a, w) ~(b, w') = (a b, w <.> w')

instance (Semiring s, Monad m) => Monad (WeightedT s m) where
  WeightedT x >>= f = WeightedT $
    x >>= (\(x,p) -> (fmap.fmap) (p<.>) (runWeightedT (f x)))
    
instance (Semiring s, Alternative m, Ord s) => Alternative (WeightedT s m) where
  empty = WeightedT ((,zero) <$> empty)
  WeightedT x <|> WeightedT y = WeightedT (x <|> y)
  
evalWeightedT :: (Semiring s, Functor f) => WeightedT s f a -> f s
evalWeightedT (WeightedT x) = fmap snd x