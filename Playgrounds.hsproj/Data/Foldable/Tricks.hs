module Data.Foldable.Tricks where
  
import Prelude hiding (dropWhile)
import Data.Foldable
import qualified Data.Map.Strict as Map


dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p = fst . foldr f ([],[]) where
  f x xs = (if p x then fst xs else ys, ys)
    where ys = x : snd xs
    

mostFrequent :: (Ord a, Foldable f) => f a -> Maybe a
mostFrequent = fmap fst . fst . foldl' f (Nothing, Map.empty) where
  f (b,m) e = (Just nb, Map.insert e c m) where
    c = maybe 1 succ (Map.lookup e m)
    nb = case b of
      Just (a,d) | d >= c -> (a,d) 
      _ -> (e,c)