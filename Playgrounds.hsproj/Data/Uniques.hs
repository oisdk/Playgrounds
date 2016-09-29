{-# language BangPatterns #-}

module Data.Uniques where
  
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Foldable  

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
