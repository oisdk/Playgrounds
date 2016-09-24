{-# language FlexibleContexts #-}

module Control.Monad.ProbTrans where
  
import Control.Monad.ListT
import Control.Monad.Writer
import Data.Ratio
import Data.Monoid

type Odds = ListT (Writer (Product Rational))

equalOdds :: Foldable f => f a -> Odds a
equalOdds xs = foldr f undefined xs (fromIntegral (length xs - 1)) where
  f y a 0 = pure y
  f y a n = ListT (writer (Just (y, a (n-1)), Product (1 % n)))

foldOdds :: (a -> Rational -> b -> b) -> b -> Odds a -> b
foldOdds f b = r where
  r = (\(x,w) -> maybe b (\(y,ys) -> f y w (r ys)) x) . fmap getProduct . runWriter . uncons
  
oddsOf :: (a -> Bool) -> Odds a -> Rational
oddsOf p = foldOdds f 0 where
  f x n r = (if p x then r + n else r) / (n + 1)
