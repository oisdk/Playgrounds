{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Church.List where

import Data.Church.Prelude
import Data.Church.Pair

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

concat :: List (List a) -> List a
concat xs = l xs (<>) nil

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
head xs = l xs (\e _ -> just e) nothing

tail :: List a -> List a
tail xs = L (\c n -> l xs (\h t g -> g h (t c)) (\_ -> n) (\_ x -> x))

instance SemiGroup (List a) where
  xs <> ys = L (\f b -> l xs f (l ys f b))

instance Monoid (List a) where
  mempty = nil
  
newtype Zip a b = Zip { unZip :: forall w. (a -> b -> w) -> w -> w }

l2 :: List a -> List b -> (a -> b -> c -> c) -> c -> c
l2 (L xs) ys c b = xs f (const b) ys where
  f e r (L l) = unZip (l tailZip nilZip) (step e r) b
  nilZip = Zip (const id)
  step e1 r1 e2 r2 = c e1 e2 (r1 r2)
  tailZip n (Zip z) = Zip (\a _ -> a n (L l)) where
    l g i = z h i where
      h t (L x) = g t (x g i)

zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f xs ys = l2 xs ys (\x y a -> f x y <: a) nil

zip :: List a -> List b -> List (Pair a b)
zip = zipWith pair