module Data.Foldable.Safe where
  
import Prelude hiding (head, last, foldl1, foldr1)
import Data.Foldable hiding (head, last, minimumBy, maximumBy, minimum, maximum, foldl1, foldr1, foldl1')
import Data.List hiding (head, last, minimumBy, maximumBy, minimum, maximum, foldl1, foldr1,foldl1')
import Data.Function.Operators

head :: Foldable f => f a -> Maybe a
head = foldr (const . Just) Nothing

last :: Foldable f => f a -> Maybe a
last = foldl' (const Just) Nothing

foldr1 :: Foldable f => (a -> a -> a) -> f a -> Maybe a
foldr1 f = foldr g Nothing where g e = Just . maybe e (f e)

foldl1' :: Foldable f => (a -> a -> a) -> f a -> Maybe a
foldl1' f = foldl' (\a e -> Just . maybe e (flip f e) $ a) Nothing

(!!) :: Foldable f => f a -> Int -> Maybe a
(!!) = foldr f (const Nothing) where
  f e _ 0 = Just e
  f _ a n = n `seq` a (n-1)

tail :: [a] -> Maybe [a]
tail [] = Nothing
tail (_:xs) = Just xs

init :: [a] -> Maybe [a]
init [] = Nothing
init (x:xs) = Just (init' x xs)
  where init' _ [] = []
        init' y (z:zs) = y : init' z zs
        
minimumBy :: Foldable f => (a -> a -> Ordering) -> f a -> Maybe a
minimumBy cmp = foldl1' $ \a e -> case cmp a e of
  GT -> e
  _  -> a
  
maximumBy :: Foldable f => (a -> a -> Ordering) -> f a -> Maybe a
maximumBy cmp = foldl1' $ \a e -> case cmp a e of
  LT -> e
  _  -> a

minimum :: (Ord a, Foldable f) => f a -> Maybe a
minimum = minimumBy compare

maximum :: (Ord a, Foldable f) => f a -> Maybe a
maximum = maximumBy compare

minimumOn :: (Ord b, Foldable f) => (a -> b) -> f a -> Maybe a
minimumOn ord = fmap fst . foldl' f Nothing where
  f Nothing e = Just (e, ord e)
  f (Just (a,o)) e = Just $ case compare o k of
    GT -> (e,k)
    _  -> (a,o)
    where k = ord e
    
maximumOn :: (Ord b, Foldable f) => (a -> b) -> f a -> Maybe a
maximumOn ord = fmap fst . foldl' f Nothing where
  f Nothing e = Just (e, ord e)
  f (Just (a,o)) e = Just $ case compare o k of
    LT -> (e,k)
    _  -> (a,o)
    where k = ord e
    
shorterThan :: Foldable f => f a -> Int -> Bool
shorterThan = foldr f (const True) where
  f _ a n = n > 1 && a (n-1)
  
longerThan :: Foldable f => f a -> Int -> Bool
longerThan = foldr f (const False) where
  f _ a n = n < 1 || a (n-1)