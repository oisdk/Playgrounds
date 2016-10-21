{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

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

instance IsNum Nat where
  fromInteger n = if n == 0 then N (\f x -> x) else succ (fromInteger (n-1))

instance SemiRing Nat where
  n + m = N (\f x -> r n f (r m f x))
  n * m = r n ((+) m) 0
  zero = 0
  one = 1
  
instance Ring Nat where
  n - m = r m pred n

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
  fromInteger = fromInteger