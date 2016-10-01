module Data.Safe where
  
import Prelude hiding (head, last, foldl1, foldr1)
import Data.Foldable hiding (head, last, minimumBy, maximumBy, minimum, maximum, foldl1, foldr1)
import Data.List hiding (head, last, minimumBy, maximumBy, minimum, maximum, foldl1, foldr1)

head :: Foldable f => f a -> Maybe a
head = foldr (const . Just) Nothing

last :: Foldable f => f a -> Maybe a
last = foldl' (const Just) Nothing

foldr1 :: Foldable f => (a -> a -> a) -> f a -> Maybe a
foldr1 f = foldr g Nothing where g e = Just . maybe e (f e)

foldl1 :: Foldable f => (a -> a -> a) -> f a -> Maybe a
foldl1 f = foldl' (\a e -> Just . maybe e (flip f e) $ a) Nothing

(!!) :: Foldable f => f a -> Int -> Maybe a
(!!) = foldr f (const Nothing) where
  f e _ 0 = Just e
  f _ a n = a (n-1)
  
tail :: [a] -> Maybe [a]
tail [] = Nothing
tail (_:xs) = Just xs

init :: [a] -> Maybe [a]
init [] = Nothing
init (x:xs) = Just (init' x xs)
  where init' _ [] = []
        init' y (z:zs) = y : init' z zs
        
minimumBy :: Foldable f => (a -> a -> Ordering) -> f a -> Maybe a
minimumBy cmp = foldl1 $ \a e -> case cmp a e of
  LT -> a
  EQ -> a
  GT -> e
  
maximumBy :: Foldable f => (a -> a -> Ordering) -> f a -> Maybe a
maximumBy cmp = foldl1 $ \a e -> case cmp a e of
  LT -> e
  EQ -> a
  GT -> a
  
minimum :: (Ord a, Foldable f) => f a -> Maybe a
minimum = minimumBy compare

maximum :: (Ord a, Foldable f) => f a -> Maybe a
maximum = maximumBy compare
