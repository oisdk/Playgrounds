{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The Free semiring.
module Data.Semiring.Free where

import           Control.Applicative (liftA2)

import           Data.Coerce

import           Data.Function (on)
import           Data.List (sort)

import           Data.Semiring
import           Data.Ord (comparing)

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

import Control.Monad hiding (join)
import Data.Monoid

import Control.Applicative


instance Semiring a => Semiring [a] where
  one = [one]
  zero = []
  [] <+> ys = ys
  xs <+> [] = xs
  (x:xs) <+> (y:ys) = (x <+> y) : (xs <+> ys)
  [] <.> _ = []
  _ <.> [] = []
  (x:xs) <.> (y:ys) =
    (x <.> y) : (map (x <.>) ys <+> map (<.> y) xs <+> (zero : (xs <.> ys)))








newtype Search a = Search [[a]] deriving Functor

instance Semiring (Search a) where
  Search xs <+> Search ys = Search (zipLong xs ys) where
    zipLong xs [] = xs
    zipLong [] ys = ys
    zipLong (x:xs) (y:ys) = (x++y) : zipLong xs ys
  Search xs <.> Search ys = Search (xs ++ ys)
  zero = Search []
  one = Search [[]]
  
instance Applicative Search where
  pure x = Search [[x]]
  (<*>) = ap

instance Monad Search where
  x >>= f = join (fmap f x)

join :: Search (Search a) -> Search a
join (Search []) = Search []
join (Search (x:xs)) = add x <+> one <.> join (Search xs)