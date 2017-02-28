module Data.Foldable.Tricks where
  
import Prelude hiding (dropWhile)
import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.StrictPair

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p = fst . foldr f ([],[]) where
  f x xs = (if p x then fst xs else ys, ys)
    where ys = x : snd xs

data StrictCounter a
  = None
  | Count {-# UNPACK #-} !Int !a

getValue :: StrictCounter a -> Maybe a
getValue None = Nothing
getValue (Count _ x) = Just x


mostFrequent :: (Ord a, Foldable f) => f a -> Maybe a
mostFrequent = getValue . fst' . foldl' f (None :*: Map.empty) where
  f (b :*: m) e = (nb :*: Map.insert e c m) where
    c = maybe 1 succ (Map.lookup e m)
    nb = case b of
      Count d a | d >= c -> Count d a
      _ -> Count c e