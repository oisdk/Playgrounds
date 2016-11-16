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

instance (Semiring r, Applicative m) => Semiring (ContT r m a) where
  one  = ContT (const (pure one))
  zero = ContT (const (pure one))
  f <+> g = ContT (\k -> liftA2 (<+>) (runContT f k) (runContT g k))
  f <.> g = ContT (\k -> liftA2 (<.>) (runContT f k) (runContT g k))
  
instance Semiring a => Semiring [a] where
  one = [one]
  zero = []
  [] <+> ys = ys
  xs <+> [] = xs
  (x:xs) <+> (y:ys) = (x <+> y) : (xs <+> ys)
  [] <.> _ = []
  _ <.> [] = []
  (x:xs) <.> (y:ys) =
    (x <.> y) : (map (x <.>) ys <+> map (<.> y) xs <+> (xs <.> ys))

instance (Monoid r, Applicative m) => Monoid (ContT r m a) where
  mempty = ContT (const (pure mempty))
  mappend (ContT f) (ContT g) = ContT (\x -> liftA2 mappend (f x) (g x))
 

newtype Bag a = Bag { getBag :: [a] } deriving (Functor, Applicative, Monad, Monoid, Foldable)

instance Monoid a => Semiring  (Bag a) where
  zero = mempty
  (<+>) = mappend
  one = pure mempty
  (<.>) = liftA2 mappend




newtype List a = List { runList :: forall m. Monoid m => Cont m a } deriving Functor
newtype LList a = LList { runLList :: forall m. Semiring m => Cont m a } deriving Functor

instance Semiring (LList a) where
  zero = LList zero
  one = LList one
  LList x <+> LList y = LList (x <+> y)
  LList x <.> LList y = LList (x <.> y)

instance Foldable LList where
  foldMap f (LList x) = fold (runCont x (Bag . pure . f))

instance Foldable List where
  foldMap = flip (runCont.runList)

instance Monoid (List a) where
  mappend (List x) (List y) = List (mappend x y)
  mempty = List mempty

bfs :: List a -> [a]
bfs a = (toList . fold . levels) (foldMap yield a) where yield x = Levels [pure x]

newtype Levels a = Levels { levels :: [List a] }

instance Monoid (Levels a) where
  mempty        = Levels []
  x `mappend` y = Levels (mempty : merge (levels x) (levels y))

instance IsList (List a) where
  type Item (List a) = a
  fromList = anyOf
  toList = foldr (:) []
  
instance Applicative List where
  pure x = List (pure x)
  (<*>) = ap

instance Applicative LList where
  pure x = LList (pure x)
  (<*>) = ap

instance Alternative List where
  empty = mempty
  (<|>) = mappend
  
instance Alternative LList where
  empty = mzero
  (<|>) = mplus

instance Monad List where
  x >>= f = foldMap f x

instance Monad LList where
  x >>= f = add (fmap f x)

instance MonadPlus List where
  mzero = mempty
  mplus = mappend

instance MonadPlus LList where
  mzero = zero
  x `mplus` y = x <+> y

anyOf :: Alternative m => [a] -> m a
anyOf = foldr ((<|>) . pure) empty 

-- like 'zipWith append' without cutting the longer list
merge :: [List a] -> [List a] -> [List a]
merge []      ys    = ys
merge xs      []    = xs
merge (x:xs) (y:ys) = mappend x y : merge xs ys

