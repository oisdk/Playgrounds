{-# LANGUAGE RankNTypes #-}

module Control.Monad.Church.State where

newtype State s a =
  State (forall c. (a -> s -> c) -> s -> c)

instance Functor (State s) where
  fmap f (State m) = State (\t -> m (t . f))
  {-# INLINABLE fmap #-}

instance Applicative (State s) where
  pure x = State (\t -> t x)
  {-# INLINABLE pure #-}
  State fs <*> State xs =
    State (\t -> fs (\f -> xs (t . f)))
  {-# INLINABLE (<*>) #-}

evalState :: State s a -> s -> a
evalState (State x) = x const
{-# INLINABLE evalState #-}

newtype List a =
  List (forall b. b -> (a -> List a -> b) -> b)

zipInto
  :: (Traversable t, Foldable f)
  => (a -> Maybe b -> c)
  -> t a
  -> f b
  -> t c
zipInto f xs =
  evalState (traverse (flip fmap pop . f) xs) . foldr cons nil where
    cons y ys = List (const (\g -> g y ys))
    nil = List const
    pop = State (\t (List l) -> l (t Nothing nil) (t . Just))
{-# INLINABLE zipInto #-}