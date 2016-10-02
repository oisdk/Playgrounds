{-# LANGUAGE RankNTypes #-}

module Data.Scott.List where
  
import Data.Function
import Data.Monoid
import Prelude hiding ((++), concat, tail, head, zip, zipWith)

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

fromList :: Foldable f => f a -> List a
fromList = foldr (\x xs -> L (\_ f -> f x xs)) nil

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
  
newtype ScottZip a b =
  ScottZip { runScottZip :: a -> (ScottZip a b -> b) -> b }

foldr2 :: (Foldable f, Foldable g) => (a -> b -> c -> c) -> c -> f a -> g b -> c
foldr2 c i xs = foldr f (const i) xs . ScottZip . foldr g (\_ _ -> i) where
 g e2 r2 e1 r1 = c e1 e2 (r1 (ScottZip r2))
 f e r (ScottZip x) = x e r
  
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f = foldr2 (\x y a -> f x y <: a) nil

zip :: List a -> List b -> List (a,b)
zip = zipWith (,)