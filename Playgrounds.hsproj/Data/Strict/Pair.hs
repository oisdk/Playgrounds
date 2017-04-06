{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Strict.Pair where
  
import Data.Bifunctor
import Data.Semiring
  
infixl 5 :!:
data a :!:  b
  = !a :!: !b
  deriving (Functor, Foldable, Eq, Ord, Show, Read)

fst' :: a :!: b -> a
fst' (x :!: _) = x

snd' :: a :!: b -> b
snd' (_ :!: y) = y

instance Bifunctor (:!:) where
  first f (x :!: y) = f x :!: y
  second f (x :!: y) = x :!: f y
  bimap f g (x :!: y) = f x :!: g y

instance (Monoid a, Monoid b) => Monoid (a :!: b) where
  mappend (xx :!: xy) (yx :!: yy) = (mappend xx yx :!: mappend xy yy)
  mempty = mempty :!: mempty
  
instance Monoid a => Applicative ((:!:) a) where
  pure = (:!:) mempty
  xm :!: xf <*> ym :!: yx = mappend xm ym :!: xf yx

instance Monoid a => Monad ((:!:) a) where
  m :!: x >>= f = let (ym :!: y) = f x in (mappend m ym :!: y)
  
instance (Semiring a, Semiring b) => Semiring (a :!: b) where
  zero = zero :!: zero
  one = one :!: one
  (xx :!: xy) <+> (yx :!: yy) = xx <+> yx :!: xy <+> yy
  (xx :!: xy) <.> (yx :!: yy) = xx <.> yx :!: xy <.> yy
  

