{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Ring
  ( module P
  , Semiring(..)
  , plusAssoc
  , mulAssoc
  , plusComm
  , mulDistribL
  , mulDistribR
  , plusId
  , mulId
  ) where
  
import Prelude as P hiding ((+), (*))
import qualified Prelude as P
import Data.Coerce
import Data.Ord
import Test.QuickCheck
import Control.Monad
import Data.Monoid
import Data.List
import Control.Arrow
import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as Set

class Semiring a where
  one :: a
  zero :: a
  infixl 6 +
  (+) :: a -> a -> a
  infixl 7 *
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

instance Semiring a => Semiring (b -> a) where
  one = const one
  zero = const zero
  (f + g) x = f x + g x
  (f * g) x = f x * g x

plusAssoc :: (Eq a, Semiring a) => a -> a -> a -> Bool
plusAssoc x y z = lp == rp where
  l = x + y
  r = y + z
  lp = l + z
  rp = x + r
  
mulAssoc :: (Eq a, Semiring a) => a -> a -> a -> Bool
mulAssoc x y z = lp == rp where
  l = x * y
  r = y * z
  lp = l * z
  rp = x * r

plusComm :: (Eq a, Semiring a) => a -> a -> Bool
plusComm x y = x + y == y + x

mulDistribL :: (Eq a, Semiring a) => a -> a -> a -> Bool
mulDistribL x y z = x * (y + z) == x * y + x * z

mulDistribR :: (Eq a, Semiring a) => a -> a -> a -> Bool
mulDistribR x y z = (x + y) * z == x * z + y * z

plusId :: (Eq a, Semiring a) => a -> Bool
plusId x = x + zero == x && zero + x == x

mulId :: (Eq a, Semiring a) => a -> Bool
mulId x = x * one == x && one * x == x

type OneTest a = a -> Bool
type TwoTest a = a -> a -> Bool
type ThreeTest a = a -> a -> a -> Bool

instance Monoid a => Semiring (ZipList a) where
  ZipList xs * ZipList ys = ZipList (xs ++ ys)
  (+) = liftA2 mappend
  zero = ZipList (repeat mempty)
  one = ZipList []
  
instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = ZipList <$> arbitrary

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary

cartProd :: (Ord a, Monoid a) => Set a -> Set a -> Set a
cartProd xs ys = Set.foldr (\x -> flip (Set.foldr (Set.insert . mappend x)) ys) Set.empty xs

instance (Ord a, Monoid a) => Semiring (Set a) where
  (*) = cartProd
  (+) = Set.union
  zero = Set.empty
  one = Set.singleton mempty
  
instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = Set.fromList <$> arbitrary

