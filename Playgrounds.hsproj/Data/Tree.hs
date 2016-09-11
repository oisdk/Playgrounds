{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}

module Data.Tree where

import Data.Functor.Recursive
  
data Tree a
  = Empty
  | Node (Tree a) a (Tree a)
  deriving (Functor, Foldable, Traversable)

data instance Unfix (Tree a) r
  = EmptyF 
  | NodeF r a r
  deriving (Functor, Foldable, Traversable)
                               
instance Recursive (Tree a) where
  project Empty = EmptyF
  project (Node l x r) = NodeF l x r
  
instance Corecursive (Tree a) where
  embed EmptyF = Empty
  embed (NodeF l x r) = Node l x r
  
instance Show a => Show (Tree a) where
  show = ("fromList " ++) . show . foldr (:) []

insert :: Ord a => a -> Tree a -> Tree a
insert x = para alg where
  alg EmptyF = Node Empty x Empty
  alg (NodeF (le,li) y (re,ri)) = case compare x y of
    LT -> Node li y re
    EQ -> Node le y re
    GT -> Node le y ri

fromList :: (Foldable f, Ord a) => f a -> Tree a
fromList = foldr insert Empty