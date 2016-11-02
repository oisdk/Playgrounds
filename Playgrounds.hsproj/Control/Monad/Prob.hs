{-# LANGUAGE TupleSections, PatternSynonyms, ViewPatterns, DeriveFunctor, DeriveFoldable #-}

module Control.Monad.Prob where

import Data.Semiring
import Control.Applicative
import Text.Parse
import Test.QuickCheck
import Prelude.Extras
import Control.Arrow (first, (***), (&&&))
import Data.Function
import Control.Applicative.Alternative
import Control.Monad
import Prelude.Extras
import Data.Ratio

newtype WeightedT s m a = WeightedT
  { runWeightedT :: m (a, s) 
  } deriving Functor
  
instance (Eq1 m, Eq s, Eq a) => Eq (WeightedT s m a) where
  WeightedT x == WeightedT y = x ==# y
  
instance (Ord1 m, Ord s, Ord a) => Ord (WeightedT s m a) where
  compare (WeightedT x) (WeightedT y) = compare1 x y

instance (Show1 m, Show s, Show a) => Show (WeightedT s m a) where
  showsPrec n (WeightedT x) =
    showParen (n>=10)
      (showString "WeightedT {runWeightedT = " . showsPrec1 0 x . showChar ')')

instance (Show1 m, Show s) => Show1 (WeightedT s m) where
  showsPrec1 n (WeightedT x) =
    showParen (n>=10)
      (showString "WeightedT {runWeightedT = " . showsPrec1 0 x . showChar ')')
  
instance (Eq1 m, Eq s) => Eq1 (WeightedT s m) where
  WeightedT x ==# WeightedT y = x ==# y
  
instance (Ord1 m, Ord s) => Ord1 (WeightedT s m) where
  compare1 (WeightedT x) (WeightedT y) = compare1 x y

instance (Semiring s, Applicative m) => Applicative (WeightedT s m) where
  pure x = WeightedT (pure (x,one))
  fs <*> xs = WeightedT $
    liftA2 k (runWeightedT fs) (runWeightedT xs)
      where k ~(a, w) ~(b, w') = (a b, w <.> w')

instance (Semiring s, Monad m) => Monad (WeightedT s m) where
  WeightedT x >>= f = WeightedT $
    x >>= (\(x,p) -> (fmap.fmap) (p<.>) (runWeightedT (f x)))
    
instance (Semiring s, Alternative m, Monad m) => Alternative (WeightedT s m) where
  empty = WeightedT ((,zero) <$> empty)
  WeightedT x <|> WeightedT y = maybe empty pure =<< WeightedT (liftA2 f (opt x) (opt y)) where
    opt m = (fmap.first) Just m <|> pure (Nothing,zero)
    f (xr,xp) (yr,yp) = (xr<|>yr,xp<+>yp)
    
  
evalWeightedT :: (Semiring s, Functor f) => WeightedT s f a -> f s
evalWeightedT (WeightedT x) = fmap snd x

probOf :: (Semiring s, Integral s) => (a -> Bool) -> WeightedT s [] a -> Ratio s
probOf e (WeightedT xs) = uncurry (%) (add [ (if e x then p else zero,one) | (x,p) <- xs])