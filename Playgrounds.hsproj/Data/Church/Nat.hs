{-# LANGUAGE RankNTypes #-}

module Data.Church.Nat where
  
import Data.Function

newtype Nat = N { r :: forall a. (a -> a) -> a -> a }

instance Show Nat where
  show n = show (r n succ 0)
  
instance Enum Nat where
  succ n = N (\f x -> f (r n f x))
  pred n = N (\f x -> r n (\g h -> h (g f)) (const x) id)
  toEnum 0 = N (\f x -> x)
  toEnum n = succ (toEnum (n-1))
  fromEnum n = r n succ 0

instance Num Nat where
  n + m = N (\f x -> r n f (r m f x))
  abs = id
  n * m = r n ((+) m) 0
  signum n = r n (const 1) 0
  fromInteger 0 = N (\f x -> x)
  fromInteger n = succ (fromInteger (n-1))
  n - m = r m pred n

inf :: Nat
inf = N (const.fix)

isZero :: Nat -> Bool
isZero n = r n (const False) True

nonZero :: Nat -> Bool
nonZero = not . isZero

instance Eq Nat where
  (==) n = r n (\f m -> nonZero m && f (pred m)) isZero
  
instance Ord Nat where
  (<=) = flip (>=)
  (>=) n = r n (\f m -> isZero m || f (pred m)) isZero