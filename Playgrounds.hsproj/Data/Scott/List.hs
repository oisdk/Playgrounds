{-# LANGUAGE RankNTypes #-}

module Data.Scott.List where
  
import Data.Function
import Data.Monoid
import Prelude hiding ((++), concat)

newtype List a = L
  { l :: forall b. b -> (a -> List a -> b) -> b }

nil :: List a
nil = L const

infixr 4 <:
(<:) :: a -> List a -> List a
(<:) x xs = L (\_ f -> f x xs)

tail :: List a -> List a
tail xs = l xs nil (const id)

instance Show a => Show (List a) where
  show = show . foldr (:) []
  
head :: List a -> Maybe a
head xs = l xs Nothing (const . Just)

instance Foldable List where
  foldr f b = g where g (L xs) = xs b (\e a -> f e (g a))
  foldMap f = g where g (L xs) = xs mempty (\e a -> f e <> g a)

instance Functor List where
  fmap f = g where g (L xs) = xs nil (\e a -> f e <: g a)
  
infixr 5 ++
(++) :: List a -> List a -> List a
(++) (L xs) ys = xs ys (\e a -> e <: (a ++ ys))

concat :: List (List a) -> List a
concat = foldr (++) nil

instance Applicative List where
  pure x = L (\_ f -> f x nil)
  fs <*> xs = concat (fmap (<$> xs) fs)
  
instance Monad List where
  xs >>= f = concat (fmap f xs)
  
instance Monoid (List a) where
  mempty = nil
  mappend = (++)
  
