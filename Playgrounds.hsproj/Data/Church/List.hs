{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE BangPatterns #-}

module Data.Church.List where

import GHC.Base (build)
import Prelude hiding (dropWhile, head, take)

type List a = forall b. (a -> b -> b) -> b -> b

tail :: List a -> List a
tail xs c n = xs (\h t g -> g h (t c)) (const n) (const id)

toList :: List a -> [a]
toList = build

fromList :: Foldable f => f a -> List a
fromList xs f b = foldr f b xs

nil :: List a
nil = const id

cons :: a -> List a -> List a
cons x xs f = f x . xs f

head :: List a -> Maybe a
head xs = xs (const . Just) Nothing

reverse :: List a -> List a
reverse = go c nil where
  go c xs ys = ys (\e a -> a . c e) id xs
  c x xs f = f x . xs f
  
toString :: Show a => List a -> String
toString = show . toList

dropWhile :: (a -> Bool) -> List a -> List a
dropWhile p ls = c where
  c (-:) e = ls f (const e) True where
      f a r b = if b && p a then r True else a -: r False
      
mapList :: (a -> b) -> List a -> List b
mapList f xs c = xs (c . f)

append :: List a -> List a -> List a
append xs ys f = xs f . ys f

concat :: List (List a) -> List a
concat xs = xs (\xs ys f -> xs f . ys f) nil

concatMap :: (a -> List b) -> List a -> List b
concatMap f xs = xs (\e a c -> f e c . a c) nil

filter :: (a -> Bool) -> List a -> List a
filter p xs f = xs (\e a -> if p e then f e a else a)

foldl' :: (a -> e -> a) -> a -> List e -> a
foldl' step init foldr = foldr (\a k !x -> k (step x a)) id init

iterate :: (a -> a) -> a -> List a
iterate f x c _ = go x where
  go y = c y (go (f y))
  
take :: Integer -> List a -> List a
take n xs f b = xs step (const b) n
  where
    step x g 0 = b
    step x g n = f x (g (n-1))

newtype WrappedZip a b
  = WrapZip { unwrapZip :: forall w. (a -> b -> w) -> w -> w }

newtype WrappedList a
  = WrapList { unwrapList :: List a }

foldr2 :: (a -> b -> c -> c) -> c -> List a -> List b -> c
foldr2 c b xs ys = xs f (const b) (WrapList ys) where
  f e r (WrapList l) = unwrapZip (l tailZip nilZip) (step e r) b
  nilZip = WrapZip (const id)
  step e1 r1 e2 r2 = c e1 e2 (r1 r2)
  tailZip n (WrapZip z) = WrapZip (\a _ -> a n (WrapList l)) where
    l g i = z h i where
      h t (WrapList x) = g t (x g i)




