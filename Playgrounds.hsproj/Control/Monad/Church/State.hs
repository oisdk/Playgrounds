{-# LANGUAGE RankNTypes #-}

module Control.Monad.Church.State where

newtype State s a =
  State { runState :: forall c. (a -> s -> c) -> s -> c }

instance Functor (State s) where
  fmap f (State m) = State (\t -> m (t . f))
  {-# INLINABLE fmap #-}

instance Applicative (State s) where
  pure x = State (\t -> t x)
  {-# INLINABLE pure #-}
  State fs <*> State xs = State (\t -> fs (\f -> xs (t . f)))
  {-# INLINABLE (<*>) #-}

instance Monad (State s) where
  State xs >>= f = State (xs . flip (runState . f)) where
  {-# INLINABLE (>>=) #-}

