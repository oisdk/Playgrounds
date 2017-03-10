{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Church.List where

import Data.Church.Prelude
import Data.Church.Pair

type List a = forall b. b -> (a -> b -> b) -> b

tail :: forall a. List a -> List a
tail xs n c = xs (const n) (\h t g -> g h (t c)) (const id)

--tail :: forall a. List a -> List a
--tail xs n c = xs (const n) (\h t g -> g h (defer \_ -> force t c)) (const force)

toList :: List a -> [a]
toList xs = xs [] (:)

fromList :: Foldable f => f a -> List a
fromList xs b f = foldr f b xs

nil :: List a
nil = const

infixr 4 <:
(<:) :: a -> List a -> List a
(<:) x xs = (\b f -> f x (xs b f))

--fromList :: Foldable f => f a -> List a
--fromList xs = L (\b f -> foldr f b xs)
--
--instance Show a => Show (List a) where
--  show = show . foldr (:) []
--
----concat :: List (List a) -> List a
----concat xs = l xs nil (<>)
--
--instance Functor List where
--  fmap f xs = L $ \b g -> l xs b (g.f)
--  
----instance Applicative List where
----  pure x = x <: nil
----  xs <*> ys = concat (fmap (\f -> fmap f ys) xs)
----
----instance Monad List where
----  xs >>= f = concat (fmap f xs)
--
--instance Foldable List where
--  foldr f b xs = l xs b f
--
----instance Traversable List where
----  traverse f xs = l xs g (pure nil) where
----    g e a = (<:) <$> f e <*> a
----    
--head :: List a -> Maybe a
--head xs = l xs nothing (\e _ -> just e)
--
--tail :: List a -> List a
--tail xs = L (\n c -> l xs (\_ -> n) (\h t g -> g h (t c)) (\_ x -> x))
--
----instance Semigroup (List a) where
----  xs <> ys = L (\f -> l xs f . l ys f)
----
----instance Monoid (List a) where
----  mempty = nil
----  mappend = (<>)
--  
--newtype Zip a b = Zip { unZip :: forall w. (a -> b -> w) -> w -> w }
--
--l2 :: List a -> List b -> (a -> b -> c -> c) -> c -> c
--l2 xs ys c b = xs (const b) f ys where
--  f e r l = unZip (l nilZip tailZip) (step e r) b
--  nilZip = Zip (const id)
--  step e1 r1 e2 r2 = c e1 e2 (r1 r2)
--  tailZip n (Zip z) = Zip (\a _ -> a n l) where
--    l i g = z h i where
--      h t x = g t (x i g)
--
--zipWith :: (a -> b -> c) -> List a -> List b -> List c
--zipWith f xs ys = xs ys (\x y a -> f x y <: a) nil
--
--zip :: List a -> List b -> List (Pair a b)
--zip = zipWith pair