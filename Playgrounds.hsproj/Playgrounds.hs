{-# LANGUAGE RankNTypes #-}

module Control.Monad.Church.State where

import qualified Control.Monad.State as S
import Control.Monad
import Control.Monad.Identity

newtype State s a = S { rs :: forall b. List s -> (List s -> a -> b) -> b }
newtype List a = L { l :: forall b. b -> (a -> List a -> b) -> b }
--
--(<$$>) :: (a -> b) -> a -> b
--(<$$>) f x = ($x) f
--
--instance Functor (State s) where
--  fmap f x = S (\s -> rs x s <$$> (\s x c -> c s (f x)))
--
--instance Applicative (State s) where
--  pure x = S $ \s -> pure $ \c -> c s x
--  f <*> x = S (\s -> join (rs f s <$$> (\s f -> rs x s <$$> (\s x c -> c s (f x)))))
--
--instance Monad (State s) where
--  x >>= f = S (\s -> join (rs x s <$$> (\s x -> rs (f x) s)))
--
----type State s a = StateT s Identity a
----
----runState :: s -> State s a -> (s,a)
----runState s st = runIdentity (rs st s) (,)
----
----evalState :: s -> State s a -> a
----evalState s st = runIdentity (rs st s) (\_ x -> x)
----
----execState :: s -> State s a -> s
----execState s st = runIdentity (rs st s) (\x _ -> x)
----
----get :: Applicative m => StateT s m s
----get = S $ \s -> pure (\c -> c s s)
----
----put :: Applicative m => s -> StateT s m ()
----put s = S $ \_ -> pure (\c -> c s ())
----
----modify :: Applicative m => (s -> s) -> StateT s m ()
----modify f = S r where
----  r s = pure (\c -> c (f s) ())