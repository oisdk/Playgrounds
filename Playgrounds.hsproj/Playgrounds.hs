{-# LANGUAGE GADTs, PolyKinds, DataKinds, TypeOperators, TypeFamilies, UndecidableInstances, RankNTypes, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, ExistentialQuantification, FlexibleContexts, ScopedTypeVariables, RankNTypes, ConstraintKinds, DeriveFunctor #-}

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio
import Prelude hiding (Either(..)) -- hiding (Functor(..), Applicative(..), (<$>))
import qualified Prelude
import GHC.Exts

newtype Prob s a = Prob { runProb :: [(a,s)] } deriving Prelude.Functor

class Functor f where
--  type Suitable f a :: Constraint
--  fmap :: Suitable f b => (a -> b) -> f a -> f b
--  
--class Functor f => Applicative f where
--  lower :: Suitable f a => Free f a -> f a

instance Num s => Prelude.Applicative (Prob s) where
  pure x = Prob [(x,1)]
  Prob fs <*> Prob xs
    = Prob
    [ (f x, fp * xp)
    | (f,fp) <- fs
    , (x,xp) <- xs]

newtype Reified s a = Reified { runReified :: Map a s }

data Free f xs a where
  Pure :: a -> Free 'Empty f a
  Ap :: Free xs f (b -> a) -> f ys b -> Free (Node ys b xs) f a
  
data Tree a = Empty | Node (Tree a) a (Tree a)

data Memo f xs a
  = Memo
  { run :: f a
  , cmp :: Free xs (Memo f) a }
  
data Elem (x :: k) (xs :: Tree k) :: * where
  Here :: Elem x (Node xs x ys)
  Left :: Elem x xs -> Elem x (Node xs y ys)
  Right :: Elem x ys -> Elem x (Node xs y ys)
  
back :: Num s => (forall a. f a -> (a -> s)) -> Elem x xs -> Memo f xs a -> f x
back t prf (Memo r c) = go t prf c r where
  go :: (forall a. f a -> (a -> s)) -> Elem x xs -> Free xs (Memo f) a -> f a -> f x
  go t Here (Ap _ xs) _ = run xs
  go t (Left prf) (Ap _ xs) _ = back t prf xs
--  go (Right prf) (Ap f _) _ = 
    
--instance Functor f => Functor (Memo f) where
--  fmap f (Memo r c) = 

--instance Prelude.Functor (Free f) where
--  fmap f (Pure a) = Pure (f a)
--  fmap f (Ap x y) = Ap ((f .) Prelude.<$> x) y
--
--instance Prelude.Applicative (Free f) where
--  pure = Pure
--  Pure f <*> y = Prelude.fmap f y
--  Ap x y <*> z = Ap (flip Prelude.<$> x Prelude.<*> z) y
--
--data Graph f a
--  = Graph
--  { comp :: Free (Graph f) a
--  , infer :: f a }
--
--instance Functor f => Functor (Graph f) where
--  type Suitable (Graph f) a = Suitable f a
--  fmap f g = Graph (fmap f (comp
--
--reify :: (Num s, Ord a) => Prob s a -> Reified s a
--reify = Reified . Map.fromListWith (+) . runProb
--
--reflect :: Reified s a -> Prob s a
--reflect = Prob . Map.toList . runReified
--
--runAp :: Prelude.Applicative g => (forall x. f x -> g x) -> Free f a -> g a
--runAp u (Pure x) = Prelude.pure x
--runAp u (Ap f x) = runAp u f Prelude.<*> u x
--
--runApC :: (forall x. f x -> g x) -> Free f a -> Free g a
--runApC u (Pure x) = Pure x
--runApC u (Ap f x) = Ap (runApC u f) (u x)
--
--
--runInfer :: Ord a => Free (Reified Rational) a -> Reified Rational a
--runInfer = reify . runAp reflect


--die :: FactorGraph (Reified Rational) Integer
--die 
--  = lift (Graph die ((Reified . Map.fromList) [(1,1/6),(2,1/6),(3,1/6),(4,1/6),(5,1/6),(6,1/6)]))
--  
--infer' 
--
--lift :: f a -> Free f a
--lift = Ap (Pure id)
--

