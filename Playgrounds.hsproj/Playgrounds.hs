{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, TypeFamilies #-}
{-# LANGUAGE OverloadedLists, FlexibleContexts, MonadComprehensions, DeriveFunctor #-}

import Data.Monoid
import Control.Monad.Cont
import GHC.Exts
import Control.Monad
import Control.Applicative
import Data.Foldable (fold)

class Semiring a where
  zero :: a
  one  :: a
  infixl 7 <.>
  (<.>) :: a -> a -> a
  infixl 6 <+>
  (<+>) :: a -> a -> a

newtype Add a = Add { getAdd :: a } deriving Semiring

instance Semiring a => Monoid (Add a) where
  mappend (Add x) (Add y) = Add (x <+> y)
  mempty = Add zero
  
add :: (Foldable f, Semiring a) => f a -> a
add = getAdd . foldMap Add

instance Monoid a => Semiring (Endo a) where
  Endo f <+> Endo g = Endo (\x -> f x <> g x)
  zero = Endo (const mempty)
  one = Endo id
  Endo f <.> Endo g = Endo (f . g)

trips :: ( MonadPlus m
         , IsList (m Integer)
         , Enum (Item (m Integer))
         , Num (Item (m Integer)))
      => m (Integer,Integer,Integer)
trips = [ (x,y,z) | x <- [1..], y <- [1..], z <- [1..], x*x + y*y == z*z ]

instance (Monoid r, Applicative m) => Monoid (ContT r m a) where
  mempty = ContT (const (pure mempty))
  mappend (ContT f) (ContT g) = ContT (\x -> liftA2 mappend (f x) (g x))

newtype List a = List { runList :: forall m. Monoid m => Cont m a } deriving Functor

instance Foldable List where
  foldMap = flip (runCont.runList)
  
instance Show a => Show (List a) where
  show = show . foldr (:) []
  

instance Monoid (List a) where
  mappend (List x) (List y) = List (mappend x y)
  mempty = List mempty
  
instance Monoid a => Semiring (List a) where
  zero = mempty
  (<+>) = mappend
  (<.>) = liftA2 mappend
  one = pure mempty

bfs :: List a -> [a]
bfs a = (toList . fold . levels) (anyOf a) where yield x = Levels [pure x]

newtype Levels a = Levels { levels :: [List a] } deriving Functor

instance Applicative Levels where
  pure x = Levels [pure x]
  Levels fs <*> Levels xs = Levels [ f <*> x | f <- fs, x <- xs ]
  
instance Alternative Levels where
  empty = Levels []
  Levels x <|> Levels y = Levels (mempty : merge x y)

instance IsList (List a) where
  type Item (List a) = a
  fromList = anyOf
  toList = foldr (:) []
  
instance Applicative List where
  pure x = List (pure x)
  (<*>) = ap

instance Alternative List where
  empty = mempty
  (<|>) = mappend

instance Monad List where
  x >>= f = foldMap f x

instance MonadPlus List where
  mzero = mempty
  mplus = mappend

anyOf :: (Alternative m, Foldable f) => f a -> m a
anyOf = getAlt . foldMap (Alt . pure)

-- like 'zipWith append' without cutting the longer list
merge :: [List a] -> [List a] -> [List a]
merge []      ys    = ys
merge xs      []    = xs
merge (x:xs) (y:ys) = mappend x y : merge xs ys

