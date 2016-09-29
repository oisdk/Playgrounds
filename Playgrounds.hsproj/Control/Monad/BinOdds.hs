{-# LANGUAGE BangPatterns, PatternSynonyms, ViewPatterns, DeriveFunctor, DeriveFoldable #-}

module Control.Monad.BinOdds where

import Data.Ratio
import Control.Arrow
import Data.Foldable
import Data.Monoid
import Data.List
import Control.Applicative
import Control.Monad
import Data.Uniques

data Odds a = Certain a
            | Odds (Odds a) Rational (Odds a)
            deriving (Eq, Functor, Foldable, Show)

pattern n :% d <- ((numerator >>> fromInteger) &&& (denominator >>> fromInteger) -> (n, d))

foldOdds :: (b -> Rational -> b -> b) -> (a -> b) -> Odds a -> b
foldOdds f b = r where
  r (Certain x) = b x
  r (Odds xs p ys) = f (r xs) p (r ys)
  
unfoldOdds :: (b -> Either a (b,Rational,b)) -> b -> Odds a
unfoldOdds f = r where
  r b = case f b of
    Left a -> Certain a
    Right (x,p,y) -> Odds (r x) p (r y)
  
fi :: Bool -> a -> a -> a
fi True  t _ = t
fi False _ f = f

probOf :: Eq a => a -> Odds a -> Rational
probOf e = foldOdds f b where
  b x = fi (e == x) 1 0
  f x (n:%d) y = (x * n + y * d) / (n + d)

equalOdds :: [a] -> Maybe (Odds a)
equalOdds [] = Nothing
equalOdds xs = Just (unfoldOdds f (xs,length xs)) where
  f ([x],_) = Left x
  f (xs,n) = Right ((ys,l), fromIntegral l % fromIntegral r, (zs,r)) where
    l = n `div` 2
    r = n - l
    (ys,zs) = splitAt l xs

fromDistrib :: [(a,Integer)] -> Maybe (Odds a)
fromDistrib [] = Nothing
fromDistrib xs = Just (unfoldOdds f (xs,length xs)) where
  f ([(x,_)],_) = Left x
  f (xs,n) = Right ((ys,l), tots ys % tots zs , (zs,r)) where
    l = n `div` 2
    r = n - l
    (ys,zs) = splitAt l xs
  tots = sum . map snd
  
toSorted :: Ord a => [a] -> Maybe (Odds a)
toSorted [] = Nothing
toSorted xs = Just (unfoldOdds f xs) where
  f [x] = Left x
  f (x:xs) = case partition (<x) xs of
    ([],ys) -> Right ([x], 1 % fromIntegral (length ys),ys)
    (xs,[]) -> Right (xs, fromIntegral (length xs), [x])
    (xs,ys) -> Right (xs, fromIntegral (length xs) % fromIntegral (length ys + 1), x:ys)

flatten :: Odds (Odds a) -> Odds a
flatten = foldOdds Odds id

instance Applicative Odds where
  pure = Certain
  fs <*> xs = flatten (fmap (<$> xs) fs)
  
instance Monad Odds where
  x >>= f = flatten (f <$> x)
  
compress :: Ord a => Odds a -> Odds a
compress xs = let Just ys = (fromDistrib . counts) xs in ys