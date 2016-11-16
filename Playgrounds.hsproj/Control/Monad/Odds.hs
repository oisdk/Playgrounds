{-# LANGUAGE DeriveFunctor #-}

module Control.Monad.Odds where
  
import Control.Comonad.Cofree

data Perhaps a
  = Never
  | With Double a
  deriving (Functor, Show, Eq, Ord)

type Odds = Cofree Perhaps