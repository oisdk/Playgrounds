module Data.FoldTricks where
  
import Prelude hiding (dropWhile)


dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p = fst . foldr f ([],[]) where
  f x xs = (if p x then fst xs else ys, ys)
    where ys = x : snd xs
    
liftA2F :: (a -> b -> c) -> [a] -> [b] -> [c]
liftA2F f xs ys = foldr (\e a -> map (f e) ys ++ a) [] xs