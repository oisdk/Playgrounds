{-# language DeriveFunctor #-}
{-# language TypeFamilies  #-}
{-# language PatternSynonyms #-}

module Data.Deque where

import           Data.Monoid ((<>))
import           Control.Monad (ap)
import           Data.Foldable hiding (toList)
import qualified Data.Foldable as Foldable
import           Control.Applicative.Backwards
import           GHC.Exts
import           Prelude hiding (reverse)
import qualified Data.List as List

infixr 4 :-:
data Deque a = [a] :-: [a] deriving (Functor, Show)

instance Foldable Deque where
  foldr f b (xs :-: ys) = foldr f (foldl' (flip f) b ys) xs
  foldMap f (xs :-: ys) = foldMap f xs <> foldl' (\a e -> f e <> a) mempty ys

instance Traversable Deque where
  traverse f (xs :-: ys) =
    (:-:) <$> traverse f xs
          <*> forwards (traverse (Backwards . f) ys)

cons :: a -> Deque a -> Deque a
cons x (xs:-:ys) = (x:xs) :-: ys

snoc :: a -> Deque a -> Deque a
snoc y (xs:-:ys) = xs :-: (y:ys)

uncons :: Deque a -> Maybe (a, Deque a)
uncons ((x:xs) :-: ys) = Just (x, (xs:-:ys))
uncons ([]:-:[]) = Nothing
uncons ([]:-:ys) = uncons (List.reverse ys :-: [])

unsnoc :: Deque a -> Maybe (a, Deque a)
unsnoc (xs :-: (y:ys)) = Just (y, (xs:-:ys))
unsnoc ([]:-:[]) = Nothing
unsnoc (xs:-:[]) = uncons ([] :-: List.reverse xs)

instance Monoid (Deque a) where
  mempty = [] :-: []
  (ws:-:xs) `mappend` (ys:-:zs) = ws ++ List.reverse xs :-: zs ++ List.reverse ys

instance Applicative Deque where
  pure x = [x] :-: []
  (<*>) = ap

instance Monad Deque where
  (>>=) = flip foldMap

instance IsList (Deque a) where
  type Item (Deque a) = a
  fromList xs = xs :-: []
  toList = Foldable.toList

reverse :: Deque a -> Deque a
reverse (xs :-: ys) = ys :-: xs