{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Data.Functor.Recursive where

import Control.Arrow
import Data.Coerce
import Control.Monad

type family Unfixed (r :: *) :: * -> *

newtype Fix f = Fix { unFix :: f (Fix f) }
type instance Unfixed (Fix f) = f

class Functor (Unfixed r) => Recursive r where
  {-# MINIMAL project #-}
  project :: r -> Unfixed r r
  cata :: (Unfixed r a -> a) -> r -> a
  cata alg = f where f = alg . fmap f . project
  para :: (Unfixed r (r,a) -> a) -> r -> a
  para alg = f where f = alg . fmap (\r -> (r, f r)) . project
  zygo :: (Unfixed r a -> a) -> (Unfixed r (a,b) -> b) -> r -> b
  zygo palg alg = snd . f where f = (palg . fmap fst &&& alg) . fmap f . project

class Functor (Unfixed r) => Corecursive r where
  {-# MINIMAL embed #-}
  embed :: Unfixed r r -> r
  ana :: (a -> Unfixed r a) -> a -> r
  ana alg = f where f = embed . fmap f . alg

instance Functor f => Recursive (Fix f) where
  project = coerce
  
instance Functor f => Corecursive (Fix f) where
  embed = coerce

-- | A monadic catamorphism.
cataM
  :: (Recursive t, Traversable (Unfixed t), Monad m)
  => (Unfixed t a -> m a) -- ^ a monadic (Base t)-algebra
  -> t                 -- ^ fixed point
  -> m a               -- ^ result
cataM f = c where c = f <=< (traverse c . project)

-- | A monadic anamorphism
anaM
  :: (Corecursive t, Traversable (Unfixed t), Monad m)
  => (a -> m (Unfixed t a))        -- ^ a monadic (Base t)-coalgebra
  -> a                          -- ^ seed
  -> m t
anaM g = a where a = fmap embed . traverse a <=< g

-- | A catamorphism which works on two structures at once. 
-- it can be used to encode "zip-like" functions.
zipo :: (Recursive g, Recursive h) 
     => (Unfixed g (h -> c) -> Unfixed h h -> c) -- ^ An algebra for two Foldables
     -> g                                  -- ^ first fixed point
     -> h                                  -- ^ second
     -> c                                  -- ^ result
zipo alg = cata zalg where zalg x = alg x . project

data ListF a r = 
  Nil | Cons a r deriving (Functor, Foldable, Traversable)
  
type instance Unfixed [a] = ListF a
  
instance Recursive [a] where
  project [] = Nil
  project (x:xs) = Cons x xs

zip' :: [a] -> [b] -> [(a,b)]
zip' = zipo alg where
  alg Nil _ = []
  alg _ Nil = []
  alg (Cons x xs) (Cons y ys) = (x,y) : xs ys
 