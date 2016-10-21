module HS99.HS1_10 where

import Data.Foldable (foldl')
  
last :: Foldable f => f a -> Maybe a
last = foldl' (\_ -> Just) Nothing


--isPalindrome [] = True
--isPalindrome [_] = True

rev [] = []
rev (x:xs) = f [x] xs where
  f ys (z:zs) = f (z:ys) zs
  f ys [] = ys
  
pal :: (Foldable f, Eq a) => f a -> Bool
pal = uncurry (==) . flip (foldr f (\ys -> (ys,[]))) [] where
  f x a ys = fmap (x:) (a (x:ys))