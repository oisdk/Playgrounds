{-# language FlexibleContexts #-}

module Control.Monad.ProbTrans where
  
import Control.Monad.ListT
import Control.Monad.Writer
import Data.Ratio
import Data.Monoid

type Odds = ListT (Writer (Product Rational))

equalOdds :: Foldable f => f a -> Odds a
equalOdds xs = foldr f undefined xs (fromIntegral $ length xs - 1) where
  f y a 0 = pure y
  f y a n = ListT (writer (Just (y, a (n-1)), Product (1 % n)))
