{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Church.Pair where
  
import Data.Church.Prelude
  
newtype Pair a b = P { p :: forall c. (a -> b -> c) -> c }

instance (Show a, Show b) => Show (Pair a b) where
  show x = p x (\y z -> "(" ++ show y ++ "," ++ show z ++ ")")
  
instance Functor (Pair a) where
  fmap f x = p x (\y z -> P (\c -> c y (f z)) )
  
fst :: Pair a b -> a
fst x = p x (\y _ -> y)

snd :: Pair a b -> b
snd x = p x (\_ y -> y)

pair :: a -> b -> Pair a b
pair x y = P (\c -> c x y)

instance (Eq a, Eq b) => Eq (Pair a b) where
  P p == P q = p $ \w x -> q $ \y z -> w == y && x == z
  
instance (Ord a, Ord b) => Ord (Pair a b) where
  compare (P p) (P q) = p $ \w x -> q $ \y z -> compare w y <> compare x z