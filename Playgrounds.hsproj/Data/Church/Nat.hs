{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Church.Nat where
  
import Prelude (String, id, (.), const, ($), otherwise, flip)
import Data.Function (fix)
import qualified Prelude
import Data.Church.Bool

type Nat = forall a. (a -> a) -> (a -> a)
newtype WrappedNat = WrapNat { unwrapNat :: Nat }
type BoundNat a = (a -> a) -> (a -> a)

pZero, pTwo :: Prelude.Integer
pZero = Prelude.read "0"
pTwo = Prelude.read "2"

cZero, cOne, cTwo :: Nat
cZero _ x = x
cOne f x = f x
cTwo f x = f (f x)

showN :: Nat -> String
showN n = Prelude.show (n Prelude.succ pZero)

succ :: Nat -> Nat
succ n f x = f (n f x)

pred :: Nat -> Nat
pred n f x = n (\g h -> h (g f)) (const x) id

infixl 6 +
(+) :: Nat -> Nat -> Nat
(+) n m f = n f . m f

infixl 7 *
(*) :: Nat -> Nat -> Nat
(*) n m = n . m

infixl 6 -
(-) :: Nat -> Nat -> Nat
(-) n m = unwrapNat (m p (WrapNat n)) where
  p (WrapNat x) = WrapNat (pred x)

infixr 8 ^
(^) :: Nat -> Nat -> Nat
(^) = flip ($)

fromInteger :: Prelude.Integer -> Nat
fromInteger n = go (Prelude.abs n) where
  go :: Prelude.Integer -> Nat
  go m | m Prelude.== pZero = (\_ x -> x)
       | Prelude.odd m = succ r
       | otherwise = r
       where r = cTwo * go (m `Prelude.quot` pTwo)

isZero :: Nat -> Bool
isZero n = n (const false) true

nonZero :: Nat -> Bool
nonZero n = n (const true) false

infix 4 ==
(==) :: Nat -> Nat -> Bool
(==) n m t f = n (\c (WrapNat m) -> m (const (c (WrapNat (pred m)))) f) (\(WrapNat n) -> n (const f) t) (WrapNat m)

infix 4 <=
(<=) :: Nat -> Nat -> Bool
(<=) n m = m >= n
  
infix 4 >=
(>=) :: Nat -> Nat -> Bool
(>=) n m t f = n (\c (WrapNat m) -> m (const (c (WrapNat (pred m)))) t) (\(WrapNat m) -> m (const f) t) (WrapNat m)

infixl 7 /
(/) :: Nat -> Nat -> Nat
(/) n = divide1 (succ n)

divide1 :: Nat -> Nat -> Nat
divide1 = fix (\d1 n m f x -> (\(WrapNat d) -> isZero d (0 f x) (f (d1 d m f x))) (WrapNat (n - m)))

