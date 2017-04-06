{-# LANGUAGE RankNTypes #-}

module Data.Church.Binary where
  
import Prelude ((.), id, flip)
import qualified Prelude
import Data.Church.Bool
import Data.Church.List

type Binary = List Bool

pTwo, pZero :: Prelude.Integer
pTwo = Prelude.read "2"
pZero = Prelude.read "0"

toIntegerBin :: Binary -> Prelude.Integer
toIntegerBin xs = xs (\e a -> (e Prelude.succ id) (a Prelude.* pTwo)) pZero

--(+) :: Binary -> Binary -> Binary
--(+)