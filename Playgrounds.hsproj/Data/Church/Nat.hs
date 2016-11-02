{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RebindableSyntax #-}

module Data.Church.Nat where
  
import Data.Church.Prelude
import qualified Prelude as NonChurch

newtype Nat = N { r :: forall a. (a -> a) -> a -> a }

instance Show Nat where
  show n = show (r n succ 0)
  
instance Enum Nat where
  succ n = N (\f x -> f (r n f x))
  pred n = N (\f x -> r n (\g h -> h (g f)) (const x) id)
  toEnum n = if n == 0 then N (\f x -> x) else succ (toEnum (n-1))
  fromEnum n = r n succ 0

inf :: Nat
inf = N (const.fix)

isZero :: Nat -> Bool
isZero n = r n (const false) true

nonZero :: Nat -> Bool
nonZero = not . isZero

instance Eq Nat where
  (==) n = r n (\f m -> nonZero m && f (pred m)) isZero

instance Ord Nat where
  (<=) = flip f where
    f n = r n (\f m -> isZero m || f (pred m)) isZero
    
instance NonChurch.Num Nat where
  fromInteger n = if 0 == n then N (\_ x -> x) else succ (fromInteger (n-1))
  N n + N m = N $ \f -> n f . m f
  N n * N m = N (n . m)
  abs = id
  signum (N n) = n (const 1) 0
  n - N m = m pred n