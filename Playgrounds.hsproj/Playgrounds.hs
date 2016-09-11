import Data.Foldable

count :: Foldable f => (a -> Bool) -> f a -> Int
count p xs = foldl' f 0 xs where
  f a e | p e = a + 1
        | otherwise = a