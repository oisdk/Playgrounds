{-# LANGUAGE RankNTypes #-}

module Data.Church.Pair where
  
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