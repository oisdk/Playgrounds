{-# LANGUAGE PolyKinds #-}

module Flip where

import Data.Bifunctor
  
newtype Flip (f :: k1 -> k2 -> *) (a :: k2) (b :: k1)
  = Flip
  { unFlip :: f b a
  } deriving (Show, Read, Eq, Ord)

instance Bifunctor f => Functor (Flip f b) where
  fmap f (Flip x) = Flip (first f x)
  
instance Bifunctor f => Bifunctor (Flip f) where
  bimap  f g (Flip x) = Flip (bimap  g f x)
  first  f   (Flip x) = Flip (second   f x)
  second f   (Flip x) = Flip (first    f x)
  
