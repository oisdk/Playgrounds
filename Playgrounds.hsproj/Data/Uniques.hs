{-# language BangPatterns #-}

module Data.Uniques where
  
import Data.Set
import Data.Monoid



--liftA2F :: (a -> b -> c) -> [a] -> [b] -> [c]
--liftA2F f xs ys = foldr (\e a -> map (f e) ys ++ a) [] xs