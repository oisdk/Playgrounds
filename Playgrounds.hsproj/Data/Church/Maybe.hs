{-# LANGUAGE RankNTypes #-}

module Data.Church.Maybe where
  
import Prelude hiding (Maybe(..))

newtype Maybe a = M { m :: forall b. b -> (a -> b) -> b }

nothing :: Maybe a
nothing = M (\d _ -> d)

just :: a -> Maybe a
just x = M (\_ f -> f x)

instance Show a => Show (Maybe a) where
  show x = m x "nothing" (\y -> "just " ++ show y)

instance Functor Maybe where
  fmap f x = M (\d c -> m x d (c.f))

instance Applicative Maybe where
  pure = just
  f <*> x = m f nothing (\g -> fmap g x)
  
instance Monad Maybe where
  x >>= f = m x nothing f