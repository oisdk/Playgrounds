module Data.Function.Utils where

converge :: Eq a => (a -> a) -> a -> a
converge f = r where
  r x | x == y = y
      | otherwise = r y
      where y = f x