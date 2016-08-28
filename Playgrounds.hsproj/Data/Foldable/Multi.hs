module Data.Foldable.Multi where
  
import Data.Coerce

newtype RecFold a b = 
  RecFold { runRecFold :: a -> (RecFold a b -> b) -> b }

type UnRecFold a b = a -> (RecFold a b -> b) -> b

--foldr2 :: (Foldable f, Foldable g) => (a -> b -> c -> c) -> c -> f a -> g b -> c
--foldr2 c i xs = foldr f (\_ -> i) xs . RecFold . foldr g (\_ _ -> i) where
--  g e2 r2 e1 r1 = c e1 e2 (r1 (RecFold r2))
--  f e r x = runRecFold x e r

foldr2 :: (Foldable f, Foldable g) => (a -> b -> c -> c) -> c -> f a -> g b -> c
foldr2 c i xs = foldr f (\_ -> i) xs . RecFold .# foldr g (\_ _ -> i) where
  g e2 r2 e1 r1 = c e1 e2 (c' r1 r2) where
    c' :: (RecFold a1 b1 -> c) -> (UnRecFold a1 b1 -> c)
    c' = coerce
    {-# INLINE c' #-}
  {-# INLINE g #-}
  g' e2 r2 e1 r1 = c e1 e2 (r1 r2)
  f = c' (\e r x -> x e r) where
    c' :: (a -> (RecFold a b -> b) -> UnRecFold a b -> b)
       -> (a -> (RecFold a b -> b) -> RecFold a b -> b)
    c' = coerce
    {-# INLINE c' #-}
  {-# INLINE f #-}
{-# INLINE foldr2 #-}
  
zipWith' :: (Foldable f, Foldable g) => (a -> b -> c) -> f a -> g b -> [c]
zipWith' f = foldr2 (\a b c -> f a b : c) []

zip' :: (Foldable f, Foldable g) => f a -> g b -> [(a,b)]
zip' = zipWith' (,)

infixr 9 .#
(.#) :: Coercible a b => (a -> b) -> (c -> a) -> c -> b
(.#) _ f = coerce f