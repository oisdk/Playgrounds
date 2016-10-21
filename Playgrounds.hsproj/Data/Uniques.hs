{-# language BangPatterns #-}

module Data.Uniques where
  
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Foldable  
import Control.Monad
import Data.Traversable
import Data.Traversable.Extras
import Control.Applicative
import Control.Monad.State.Strict

counts :: (Ord a, Foldable f, Num n) => f a -> [(a,n)]
counts = 
  Map.assocs . foldl' (\a e -> Map.insertWith (+) e 1 a) Map.empty

