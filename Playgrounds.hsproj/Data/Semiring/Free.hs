{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Data.Semiring.Free where
  
import Data.Semiring
import Control.Applicative (liftA2)

newtype Free a = Free
  { getFree :: [[a]]
  } deriving (Eq, Ord, Show, Foldable, Functor, Traversable)
  
instance Applicative Free where
  pure = Free . pure . pure
  Free fs <*> Free xs = Free (liftA2 (<*>) fs xs)
  
instance Semiring (Free a) where
  Free xs <+> Free ys = Free (xs ++ ys)
  Free xs <.> Free ys = Free (liftA2 (++) xs ys)
  one = Free [[]]
  zero = Free []
  
instance Monoid (Free a) where
  mempty = zero
  mappend = (<+>)
  
-- | `Free` is left adjoint to the forgetful functor from `Semiring`s to types.
liftFree :: Semiring s => (a -> s) -> Free a -> s
liftFree f = unFree . fmap f

unFree :: Semiring s => Free s -> s
unFree = add . fmap mul . getFree

compress :: Semiring s => Free s -> Free s
compress = pure . unFree