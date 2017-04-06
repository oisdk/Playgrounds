{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Data.Foldable.Tricks where
  
import Prelude hiding (dropWhile, zipWith, head)
import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.Strict.Pair
import Data.Strict.Counter
import GHC.Base (build)
import Data.Foldable.Safe
import Data.Functor.Compose
import Data.Bifunctor
import Data.Coerce.Helpers
import Data.Function
import Control.Arrow ((***), app)
import Data.Monoid
import Data.Functor.Compose

dropWhile :: Foldable f => (a -> Bool) -> f a -> [a]
dropWhile p ls = build c where
  c (-:) e = foldr f (const e) ls True where
      f a r b = if b && p a then r True else a -: r False

mostFrequent :: (Ord a, Foldable f) => f a -> Maybe a
mostFrequent = head . Compose .# fmap Compose .# foldl' f (Map.empty :!: Nothing) where
  f (m :!: b) e = Map.insert e c m :!: max b (Just (c :# e)) where
    c = maybe 1 succ (Map.lookup e m)

newtype Fold a = Fold { runFold :: forall b. (a -> b -> b) -> b -> b }

toFold :: Foldable f => f a -> Fold a
toFold xs = Fold (\f b -> foldr f b xs)

toFold' :: Foldable f => f a -> UnwrappedFold a
toFold' xs f b = foldr f b xs

type UnwrappedFold a = forall b. (a -> b -> b) -> b -> b

newtype Zip a b = Zip { unZip :: forall w. (a -> b -> w) -> w -> w }

l2 :: (a -> b -> c -> c) -> c -> UnwrappedFold a -> Fold b -> c
l2 c b xs ys = xs f (const b) ys where
  f e r (Fold l) = unZip (l tailZip nilZip) (step e r) b
  nilZip = Zip (const id)
  step e1 r1 e2 r2 = c e1 e2 (r1 r2)
  tailZip n (Zip z) = Zip (\a _ -> a n (Fold l)) where
    l g i = z h i where
      h t (Fold x) = g t (x g i)
      
foldr2 :: (Foldable f, Foldable g) => (a -> b -> c -> c) -> c -> f a -> g b -> c
foldr2 f b xs ys = l2 f b (toFold' xs) (toFold ys)

zipWith :: (Foldable f, Foldable g) => (a -> b -> c) -> f a -> g b -> [c]
zipWith c xs ys = build (\cons nil -> foldr2 (\x y -> cons (c x y)) nil xs ys)

unzipWith :: Foldable f => (a -> (b,c)) -> f a -> ([b],[c])
unzipWith f = foldr (uncurry bimap . bimap (:) (:) . f) ([],[])

partitionWith :: Foldable f => (a -> Either b c) -> f a -> ([b],[c])
partitionWith f = foldr (either first second . bimap (:) (:) . f) ([],[])