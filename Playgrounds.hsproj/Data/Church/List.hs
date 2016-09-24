{-# LANGUAGE RankNTypes #-}

module Data.Church.List where

import Prelude hiding ((++), concat, zip, zipWith, tail)

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
  
newtype Zip a b = Zip { unZip :: forall w. (a -> b -> w) -> w -> w }

foldr2 :: (Foldable f, Foldable g) => (a -> b -> c -> c) -> c -> f a -> g b -> c
foldr2 c b xs ys = foldr f (const b) xs (L $ \g i -> foldr g i ys) where
  f e r (L l) = unZip (l tailZip nilZip) (step e r) b
  nilZip = Zip (const id)
  step e1 r1 e2 r2 = c e1 e2 (r1 r2)
  tailZip n (Zip z) = Zip (\a _ -> a n (L l)) where
    l g i = z h i where
      h t (L x) = g t (x g i)

zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f = foldr2 (\x y a -> f x y <: a) nil

zip :: List a -> List b -> List (a,b)
zip = zipWith (,)
