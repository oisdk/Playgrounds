{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax    #-}

module Data.Church.Prelude
  ( module P
  , Semigroup(..)
  , Monoid(..)
  , Eq(..)
  , Ord(..)
  , Bool(..)
  , Num(..)
  , true
  , false
  , lt
  , eq
  , gt
  , not
  , (&&)
  , (||)
  , (>=)
  , (<)
  , (>)
  , Maybe
  , just
  , nothing
  , fix
  , on
  ) where
  

import Prelude as P
  ( Functor(..)
  , Applicative(..)
  , Monad(..)
  , Foldable(..)
  , Traversable(..)
  , (<$>)
  , (++)
  , Show(..)
  , flip
  , ($)
  , (.)
  , const
  , id
  , Integer
  , Int
  , Enum(..)
  , Num(..)
  , undefined )

import qualified Prelude as NonChurch
import Data.Function
import Data.Semigroup
import Data.Coerce
  
instance Eq Integer where
  x == y = case x NonChurch.== y of
    NonChurch.True -> true
    NonChurch.False -> false
  
instance Eq Int where
  x == y = case x NonChurch.== y of
    NonChurch.True -> true
    NonChurch.False -> false

newtype Bool = B { ifThenElse :: ∀ a. a -> a -> a }

not :: Bool -> Bool
not (B x) = B (flip x)

instance Show Bool where show b = if b then "True" else "False"

class Eq a where
  infix 4 ==
  (==) :: a -> a -> Bool
  infix 4 /=
  (/=) :: a -> a -> Bool
  x == y = not (x /= y)
  x /= y = not (x == y)
  
instance Eq Bool where
  B x == B y = B $ \t f -> x (y t f) (y f t)

newtype Ordering = O { comp :: ∀ a. a -> a -> a -> a }

instance Show Ordering where
  show o = comp o "LT" "EQ" "GT"
  
instance Eq Ordering where
  O x == O y = B $ \t f -> x (y t f f) (y f t f) (y f f t)
  
lt, eq, gt :: Ordering
lt = O $ \x _ _ -> x
eq = O $ \_ x _ -> x
gt = O $ \_ _ x -> x

class Eq a => Ord a where
  compare :: a -> a -> Ordering
  compare x y = if x <= y then (if x == y then eq else lt) else gt
  infix 4 <=
  (<=) :: a -> a -> Bool
  x <= y = comp (compare x y) true true false

infix 4 >=
(>=) :: Ord a => a -> a -> Bool
(>=) = flip (<=)

infix 4 <
(<) :: Ord a => a -> a -> Bool
x < y = not (y <= x)

infix 4 >
(>) :: Ord a => a -> a -> Bool
x > y = not (x <= y)

instance Ord Bool where
  compare (B x) (B y) = O $ \lt eq gt -> x (y eq gt) (y lt eq)

true, false :: Bool
true  = B (\t _ -> t)
false = B (\_ f -> f)

newtype Maybe a = M { m :: ∀ b. b -> (a -> b) -> b }

nothing :: Maybe a
nothing = M (\d _ -> d)

just :: a -> Maybe a
just x = M (\_ f -> f x)

instance Show a => Show (Maybe a) where
  show x = m x "nothing" (\y -> "just " ++ show y)

instance Functor Maybe where
  fmap f x = M (\d c -> m x d (c.f))

instance Applicative Maybe where
  pure = just
  f <*> x = m f nothing (\g -> fmap g x)
  
instance Monad Maybe where
  x >>= f = m x nothing f

infixr 3 &&
(&&) :: Bool -> Bool -> Bool
(&&) x y = B (\t f -> ifThenElse x (ifThenElse y t f) f)

infixr 2 ||
(||) :: Bool -> Bool -> Bool
(||) x y = B (\t -> ifThenElse x t . ifThenElse y t)

instance Semigroup Ordering where
  x <> y = comp x x y x
  
instance Monoid Ordering where
  mempty = eq
  mappend = (<>)