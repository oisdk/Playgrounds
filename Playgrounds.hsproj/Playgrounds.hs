{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Semiring-based weighting
-- | Semiring-writer
-- | Probability
-- | Heuristic search
-- | Free applicatives
-- | Is Control.Replicate a free semiring?

import Data.Semiring
import Control.Applicative
import Test.QuickCheck
import Data.Semiring.Test
import Data.Proxy
import Data.List (sort)

instance Semiring a => Semiring [a] where
  one = [one]
  zero = []
  [] <+> ys = ys
  xs <+> [] = xs
  (x:xs) <+> (y:ys) = (x <+> y) : (xs <+> ys)
  [] <.> _ = []
  _ <.> [] = []
  (x:xs) <.> (y:ys) =
    (x <.> y) : (map (x <.>) ys <+> map (<.> y) xs <+> (xs <.> ys))