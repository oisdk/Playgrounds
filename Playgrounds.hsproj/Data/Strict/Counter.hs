module Data.Strict.Counter where
  
import Data.Monoid

infixr 5 :#
data Repeated a
  = {-# UNPACK #-} !Int :# !a
  deriving (Eq, Ord, Show, Read)

instance Foldable Repeated where
  foldMap k (n :# a) = f (k a) n where
   f x y 
    | even y = f (x <> x) (y `quot` 2)
    | y == 1 = x
    | otherwise = g (x <> x) (pred y  `quot` 2) x
   g x y z 
    | even y = g (x <> x) (y `quot` 2) z
    | y == 1 = x <> z
    | otherwise = g (x <> x) (pred y `quot` 2) (x <> z)
    
instance Functor Repeated where
  fmap f (n :# x) = n :# f x
  
instance Applicative Repeated where
  pure = (:#) 1
  nf :# f <*> nx :# x = nf * nx :# f x
  
instance Monad Repeated where
  nx :# x >>= f = let ny :# y = f x in nx * ny :# y
  
