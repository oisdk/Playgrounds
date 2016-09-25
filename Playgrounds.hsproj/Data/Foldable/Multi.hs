{-# LANGUAGE RankNTypes #-}

module Data.Foldable.Multi where

import Data.Traversable

newtype ScottZip a b =
  ScottZip { runScottZip :: a -> (ScottZip a b -> b) -> b }

foldr2R :: (Foldable f, Foldable g) => (a -> b -> c -> c) -> c -> f a -> g b -> c
foldr2R c i xs = foldr f (const i) xs . ScottZip . foldr g (\_ _ -> i) where
 g e2 r2 e1 r1 = c e1 e2 (r1 (ScottZip r2))
 f e r (ScottZip x) = x e r

newtype Zip a b = Zip { unZip :: forall w. (a -> b -> w) -> w -> w }
newtype List a = List { unList :: forall b. (a -> b -> b) -> b -> b }

foldr2T :: (Foldable f, Foldable g) => (a -> b -> c -> c) -> c -> f a -> g b -> c
foldr2T c b xs ys = foldr f (const b) xs (List $ \g i -> foldr g i ys) where
  f e r (List l) = unZip (l tailZip nilZip) (step e r) b
  nilZip = Zip (const id)
  step e1 r1 e2 r2 = c e1 e2 (r1 r2)
  tailZip n (Zip z) = Zip (\a _ -> a n (List l)) where
    l g i = z h i where
      h t (List x) = g t (x g i)

newtype RecAccu a b =
  RecAccu { runRecAccu :: a -> (RecAccu a b, b) }
   
zipInto :: (Traversable t, Foldable f)
        => (a -> Maybe b -> c) -> t a -> f b -> t c
zipInto f xs =
  snd . flip (mapAccumL runRecAccu) xs . RecAccu . foldr h i where
    i e = (RecAccu i, f e Nothing)
    h e2 a e1 = (RecAccu a, f e1 (Just e2))
