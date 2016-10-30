{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Data.Ring
  ( module P
  , Semiring(..)
  , Add(..)
  , Mul(..)
  ) where
  
import Prelude as P hiding ((+), (*))
import qualified Prelude as P
import Data.Coerce

class Semiring a where
  one :: a
  zero :: a
  (+) :: a -> a -> a
  (*) :: a -> a -> a
  
instance Semiring Int where
  one = 1
  zero = 0
  (+) = (P.+)
  (*) = (P.*)
  
instance Semiring Integer where
  one = 1
  zero = 0
  (+) = (P.+)
  (*) = (P.*)
  
instance Semiring Double where
  one = 1
  zero = 0
  (+) = (P.+)
  (*) = (P.*)
  
instance Semiring Float where
  one = 1
  zero = 0
  (+) = (P.+)
  (*) = (P.*)
  
instance Semiring Bool where
  one = True
  zero = False
  (+) = (||)
  (*) = (&&)
  
newtype Add a = Add { getAdd :: a } deriving (Traversable, Eq, Ord, Show, Bounded)
newtype Mul a = Mul { getMul :: a } deriving (Traversable, Eq, Ord, Show, Bounded)

instance Functor Add where fmap = coerce

instance Functor Mul where fmap = coerce

instance Foldable Add where
  foldr   = (coerce :: ((a -> b -> c) -> (b -> a -> c)) -> (a -> b -> c) -> (b -> Add a -> c)) flip
  foldl   = coerce
  foldMap = coerce
  length  = const 1

instance Foldable Mul where
  foldr   = (coerce :: ((a -> b -> c) -> (b -> a -> c)) -> (a -> b -> c) -> (b -> Mul a -> c)) flip
  foldl   = coerce
  foldMap = coerce
  length  = const 1

instance Applicative Add where
  pure = coerce
  (<*>) = (coerce :: ((a -> b) -> a -> b) -> (Add (a -> b) -> Add a -> Add b)) ($)

instance Applicative Mul where
  pure = coerce
  (<*>) = (coerce :: ((a -> b) -> a -> b) -> (Mul (a -> b) -> Mul a -> Mul b)) ($)

instance Monad Add where
  (>>=) = flip coerce

instance Monad Mul where
  (>>=) = flip coerce
  
instance Semiring a => Monoid (Add a) where
  mempty = Add zero
  mappend = (coerce :: (a -> a -> a) -> (Add a -> Add a -> Add a)) (+)

instance Semiring a => Monoid (Mul a) where
  mempty = Mul one
  mappend = (coerce :: (a -> a -> a) -> (Mul a -> Mul a -> Mul a)) (*)

instance Semiring a => Semiring (Add a) where
  zero = Add zero
  one = Add one
  (+) = (coerce :: (a -> a -> a) -> (Add a -> Add a -> Add a)) (+)
  (*) = (coerce :: (a -> a -> a) -> (Add a -> Add a -> Add a)) (*)

instance Semiring a => Semiring (Mul a) where
  zero = Mul zero
  one = Mul one
  (+) = (coerce :: (a -> a -> a) -> (Mul a -> Mul a -> Mul a)) (+)
  (*) = (coerce :: (a -> a -> a) -> (Mul a -> Mul a -> Mul a)) (*)


