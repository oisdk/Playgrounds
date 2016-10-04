{-# language BangPatterns #-}

module Data.Uniques where
  
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Foldable  
import Control.Monad
import Data.Traversable
import Control.Applicative
import Control.Monad.State.Strict

uniques :: (Ord a, Foldable f) => f a -> [a]
uniques xs = foldr f (const []) xs Set.empty where
  f e a !s | Set.member e s = a s
           | otherwise = e : a (Set.insert e s)
           

counts :: (Ord a, Foldable f, Num n) => f a -> [(a,n)]
counts = 
  Map.assocs . foldl' ((flip . Map.alter) (Just . foldr (+) 1)) Map.empty

countsEq :: (Eq a, Foldable f, Num n) => f a -> [(a,n)]
countsEq = foldl' f [] where
  f [] e = [(e,1)]
  f ((x,n):xs) e | e == x = (x,n+1) : xs
                 | otherwise = (x,n) : f xs e

ordNub :: (Ord a, Traversable f, Monad f, Alternative f) => f a -> f a
ordNub = g <=< snd . mapAccumL f Set.empty where
  f a e = (Set.insert e a, (Set.notMember e a, e))
  g ~(b,e) = if b then pure e else empty
  
ordNubOn :: (Ord b, Traversable f, Monad f, Alternative f) => (a -> b) -> f a -> f a
ordNubOn k = g <=< snd . mapAccumL f Set.empty where
  f a e = (Set.insert v a, (Set.notMember v a, e)) where v = k e
  g ~(b,e) = if b then pure e else empty