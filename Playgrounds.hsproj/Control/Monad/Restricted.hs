{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Control.Monad.Restricted where
  
import qualified Prelude
import           Prelude hiding (Functor(..), Applicative(..), Monad(..))

import qualified Control.Applicative as Applicative

import           Data.Set (Set)
import qualified Data.Set as Set

class Functor f a b where
  fmap :: (a -> b) -> f a -> f b
  
class Pointed f a where
  pure :: a -> f a
  
class Functor f b c => Apply f a b c where
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  
class (Pointed f a, Apply f a b c) => Applicative f a b c

class Applicative f a b c => Monad f a b c where
  (>>=) :: f a -> (a -> f b) -> f b