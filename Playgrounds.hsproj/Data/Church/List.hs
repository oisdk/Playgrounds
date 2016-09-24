{-# LANGUAGE RankNTypes #-}

module Data.Church.List where

import Prelude hiding ((++), concat, zipWith, tail)

newtype List a = L { l :: forall b. (a -> b -> b) -> b -> b }

nil :: List a
nil = L (\_ b -> b)

infixr 4 <:
(<:) :: a -> List a -> List a
(<:) x xs = L (\f b -> f x (l xs f b))

fromList :: Foldable f => f a -> List a
fromList xs = L (\f b -> foldr f b xs)

instance Show a => Show (List a) where
  show = show . foldr (:) []

infixr 5 ++
(++) :: List a -> List a -> List a
(++) xs ys = L (\f b -> l xs f (l ys f b))

concat :: List (List a) -> List a
concat xs = l xs (++) nil

instance Functor List where
  fmap f xs = L $ \g b -> l xs (g.f) b
  
instance Applicative List where
  pure x = x <: nil
  xs <*> ys = concat (fmap (\f -> fmap f ys) xs)

instance Monad List where
  xs >>= f = concat (fmap f xs)

instance Foldable List where
  foldr f b xs = l xs f b

instance Traversable List where
  traverse f xs = l xs g (pure nil) where
    g e a = (<:) <$> f e <*> a
    
head :: List a -> Maybe a
head xs = l xs (\e _ -> Just e) Nothing

tail :: List a -> List a
tail xs = L (\c n -> l xs (\h t g -> g h (t c)) (\_ -> n) (\_ x -> x))

instance Monoid (List a) where
  mempty = nil
  mappend = (++)
