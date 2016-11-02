module Data.Foldable.Tricks where
  
import Prelude hiding (dropWhile)

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p = fst . foldr f ([],[]) where
  f x xs = (if p x then fst xs else ys, ys)
    where ys = x : snd xs
    