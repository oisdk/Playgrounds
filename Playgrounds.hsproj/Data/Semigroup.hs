{-# language FunctionalDependencies, FlexibleInstances #-}

module Data.Semigroup where
  
import Data.Monoid

class (Monoid add, Monoid mult)
  => Semigroup a add mult | a -> add, a -> mult where
    toAdd    :: a -> add
    fromAdd  :: add -> a
    toMult   :: a -> mult
    fromMult :: mult -> a
  
(<+>), (<.>) :: Semigroup a add mult => a -> a -> a

x <+> y = fromAdd  (toAdd  x <> toAdd  y)
x <.> y = fromMult (toMult x <> toMult y)

one, zero :: Semigroup a add mult => a
zero = fromAdd mempty
one = fromMult mempty

instance Semigroup Int (Sum Int) (Product Int) where
  toAdd    = Sum
  fromAdd  = getSum
  toMult   = Product
  fromMult = getProduct
  
instance Semigroup Bool Any All where
  toAdd    = Any
  fromAdd  = getAny
  toMult   = All
  fromMult = getAll