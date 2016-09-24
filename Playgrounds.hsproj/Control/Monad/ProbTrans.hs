{-# language FlexibleContexts #-}

module Control.Monad.ProbTrans where
  
import Control.Monad.ListT
import Control.Monad.Writer
import Data.Ratio
import Data.Monoid

type Odds = ListT (Writer (Product Rational))

equalOdds :: [a] -> Odds a
equalOdds [x] = pure x
equalOdds (x:xs) = ListT (writer ((Just (x, equalOdds xs)), Product (1 % fromIntegral (length xs))))

foldOdds :: (a -> Rational -> b -> b) -> (Rational -> b) -> Odds a -> b
foldOdds f b = r where
  r = (\(x,w) -> maybe (b w) (\(y,ys) -> f y w (r ys)) x) . fmap getProduct . runWriter . uncons
  
oddsOf :: (a -> Bool) -> Odds a -> Rational
oddsOf p = foldOdds f (const $ 1 % 2) where
  f x n r = (if p x then r + n else r) / (n + 1)
