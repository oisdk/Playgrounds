module Data.Safe where
  
import Prelude hiding (head, foldl1, foldr1)
import qualified Data.Foldable as Foldable
import Data.Maybe

head :: Foldable f => f a -> Maybe a
head = foldr (\e _ -> Just e) Nothing

foldr1 :: Foldable f => (a -> a -> a) -> f a -> Maybe a
foldr1 f = foldr g Nothing where g e = Just . maybe e (f e)

foldl1 :: Foldable f => (a -> a -> a) -> f a -> Maybe a
foldl1 f xs = foldr (\_ _ -> Just (Foldable.foldl1 f xs)) Nothing xs