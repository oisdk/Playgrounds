module Data.Strict.Maybe where
  
import Prelude hiding (Maybe(..), maybe)
import Data.Foldable

data Maybe a
  = Nothing
  | Just !a
  
instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)
  
instance Foldable Maybe where
  foldr _ b Nothing = b
  foldr f b (Just x) = f x b
  foldMap _ Nothing = mempty
  foldMap f (Just x) = f x
  length Nothing = 0
  length (Just _) = 1
  foldl _ b Nothing = b
  foldl f b (Just x) = f b x

instance Traversable Maybe where
  traverse _ Nothing = pure Nothing
  traverse f (Just x) = fmap Just (f x)
  sequence Nothing = pure Nothing
  sequence (Just x) = fmap Just x
  
instance Applicative Maybe where
  pure = Just
  Just f <*> Just x = Just (f x)
  _ <*> _ = Nothing

instance Monad Maybe where
  Nothing >>= _ = Nothing
  Just x >>= f = f x
  
maybe :: b -> (a -> b) -> Maybe a -> b
maybe b _ Nothing = b
maybe _ f (Just x) = f x 