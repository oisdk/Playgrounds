module Numeric.Bases where
  
import Data.Foldable
import Data.Unfold
import Control.Applicative.Alternative
import Data.Function.Operators
  
fromDigs :: Num a => a -> [a] -> a
fromDigs base = foldl' f 0 where
  f a e = base * a + e
  
toDigs :: (Integral a, Num a) => a -> a -> [a]
toDigs base =
  unfoldl ((`quotRem` base) <-< ensure (>0))
  
pred' :: [Int] -> [Int]
pred' (1:xs) = (0:xs)
pred' (0:xs) = 1 : pred' xs