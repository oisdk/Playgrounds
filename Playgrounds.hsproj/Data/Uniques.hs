{-# language BangPatterns #-}

module Data.Uniques where
  
import qualified Data.Set as Set
  
uniques :: (Ord a, Foldable f) => f a -> [a]
uniques xs = foldr f (const []) xs Set.empty where
  f e a !s | Set.member e s = a s
           | otherwise = e : a (Set.insert e s)