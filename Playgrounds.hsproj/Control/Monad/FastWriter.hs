{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Control.Monad.FastWriter where
  
import Control.Monad.State
import Data.Tuple

newtype Writer w a = Writer_
  { _rw :: State w a
  } deriving (Functor, Applicative, Monad)

tell :: Monoid w => w -> Writer w ()
tell = Writer_ . modify . mappend

pattern Writer w <- (swap . flip runState mempty . _rw -> w) where
  Writer (w,x) = Writer_ (state (\s -> (x, mappend s w)))
  
runWriter :: Monoid w => Writer w a -> (w,a)
runWriter (Writer w) = w

evalWriter :: Monoid w => Writer w a -> w
evalWriter (Writer (w,_)) = w

execWriter :: Monoid w => Writer w a -> a
execWriter (Writer (_,x)) = x