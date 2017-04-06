{-# LANGUAGE RankNTypes #-}

module Data.Church.These where
  
import Prelude ((.), id, flip)
import Data.Semigroup

type These a b = forall c. (a -> c) -> (b -> c) -> (a -> b -> c) -> c

merge :: (Semigroup a, Semigroup b) => These a b -> These a b -> These a b
merge xs ys this that these = xs
  (\a ->   ys (\b -> this  (a<>b)  ) (\y -> these a     y     ) (\b y -> these (a<>b) y     ))
  (\x ->   ys (\b -> these  b     x) (\y -> that (x<>y)       ) (\b y -> these b      (x<>y)))
  (\a x -> ys (\b -> these (a<>b) x) (\y -> these a     (x<>y)) (\b y -> these (a<>b) (x<>y)))

this :: a -> These a b
this x t _ _ = t x

that :: b -> These a b
that x _ t _ = t x

these :: a -> b -> These a b
these x y _ _ t = t x y

newtype WrappedThese a b
  = WrapThese
  { unwrapThese :: These a b }