{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

module Data.Functor.Recursive where

import Control.Arrow
import Control.Monad

data family Unfix (r :: *) :: * -> *

class Functor (Unfix r) => Recursive r where
  project :: r -> Unfix r r
  cata :: (Unfix r a -> a) -> r -> a
  cata alg = f where f = alg . fmap f . project
  para :: (Unfix r (r,a) -> a) -> r -> a
  para alg = f where f = alg . fmap (\r -> (r, f r)) . project
  zygo :: (Unfix r a -> a) -> (Unfix r (a,b) -> b) -> r -> b
  zygo palg alg = snd . f where f = (palg . fmap fst &&& alg) . fmap f . project

type family Fix (r :: * -> *) :: * where
  Fix (Unfix r) = r

class Functor (Unfix r) => Corecursive r where
  embed :: Unfix r r -> r
  ana :: (a -> Unfix r a) -> a -> r
  ana alg = f where f = embed . fmap f . alg

-- | A monadic catamorphism.
cataM
  :: (Recursive t, Traversable (Unfix t), Monad m)
  => (Unfix t a -> m a) -- ^ a monadic (Base t)-algebra
  -> t                 -- ^ fixed point
  -> m a               -- ^ result
cataM f = c where c = f <=< (traverse c . project)

-- | A monadic anamorphism
anaM
  :: (Corecursive t, Traversable (Unfix t), Monad m)
  => (a -> m (Unfix t a))        -- ^ a monadic (Base t)-coalgebra
  -> a                          -- ^ seed
  -> m t
anaM g = a where a = fmap embed . traverse a <=< g

-- | A catamorphism which works on two structures at once. 
-- it can be used to encode "zip-like" functions.
zipo :: (Recursive g, Recursive h) 
     => (Unfix g (h -> c) -> Unfix h h -> c) -- ^ An algebra for two Foldables
      -> g                                  -- ^ first fixed point
      -> h                                  -- ^ second
      -> c                                  -- ^ result
zipo alg = cata zalg where zalg x = alg x . project

data instance Unfix [a] r = 
  Nil | Cons a r deriving (Functor, Foldable, Traversable)
  
instance Recursive [a] where
  project [] = Nil
  project (x:xs) = Cons x xs

zip' :: [a] -> [b] -> [(a,b)]
zip' = zipo alg where
  alg Nil _ = []
  alg _ Nil = []
  alg (Cons x xs) (Cons y ys) = (x,y) : xs ys