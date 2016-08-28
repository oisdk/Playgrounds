{-# LANGUAGE RankNTypes #-}

module Control.Monad.Church.State where

import qualified Control.Monad.State as S
import Control.Monad
import Control.Monad.Identity

newtype StateT s m a = S { rs :: forall b. s -> m ((s -> a -> b) -> b) }

(<$$>) :: Functor f => f (a -> b) -> a -> f b
(<$$>) f x = fmap ($x) f

instance Functor m => Functor (StateT s m) where
  fmap f x = S (\s -> rs x s <$$> (\s x c -> c s (f x)))

instance Monad m => Applicative (StateT s m) where
  pure x = S $ \s -> pure $ \c -> c s x
  f <*> x = S (\s -> join (rs f s <$$> (\s f -> rs x s <$$> (\s x c -> c s (f x)))))

instance Monad m => Monad (StateT s m) where
  x >>= f = S (\s -> join (rs x s <$$> (\s x -> rs (f x) s)))

type State s a = StateT s Identity a

runState :: s -> State s a -> (s,a)
runState s st = runIdentity (rs st s) (,)

evalState :: s -> State s a -> a
evalState s st = runIdentity (rs st s) (\_ x -> x)

execState :: s -> State s a -> s
execState s st = runIdentity (rs st s) (\x _ -> x)

get :: Applicative m => StateT s m s
get = S $ \s -> pure (\c -> c s s)

put :: Applicative m => s -> StateT s m ()
put s = S $ \_ -> pure (\c -> c s ())

modify :: Applicative m => (s -> s) -> StateT s m ()
modify f = S r where
  r s = pure (\c -> c (f s) ())