{-# LANGUAGE BangPatterns, PatternSynonyms, ViewPatterns, DeriveFunctor, DeriveFoldable #-}

module Control.Monad.BinOdds where

import Data.Ratio
import Control.Arrow
import Data.Foldable

data Odds a = Certainly a
            | Odds (Odds a) Rational (Odds a)
            deriving (Eq, Functor, Foldable, Show)

pattern n :% d <- ((numerator &&& denominator) >>> (fromInteger *** fromInteger) -> (n, d))

foldOdds :: (b -> Rational -> b -> b) -> (a -> b) -> Odds a -> b
foldOdds f b = r where
  r (Certainly x) = b x
  r (Odds xs p ys) = f (r xs) p (r ys)
  
fi :: Bool -> a -> a -> a
fi True  t _ = t
fi False _ f = f

probOf :: Eq a => a -> Odds a -> Rational
probOf e = foldOdds f b where
  b x = fi (e == x) 1 0
  f x (n:%d) y = (x * n + y * d) / (n + d)

equalOdds :: [a] -> Maybe (Odds a)
equalOdds [] = Nothing
equalOdds xs = Just (r xs) where
  r [x] = Certainly x
  r xs = case split' xs of
    (ys,zs,Nothing) -> Odds (r ys) 1           (r zs)
    (ys,zs,Just zn) -> Odds (r ys) ((zn+1)%zn) (r zs)
    where 
      split' []       = ([ ],[],Nothing)
      split' [x]      = ([x],[],Just 0 )
      split' (x:y:xs) = let (xx,yy,rr) = split' xs in (x:xx,y:yy,succ <$> rr)

fromDistrib :: [(a,Integer)] -> Maybe (Odds a)
fromDistrib [] = Nothing
fromDistrib xs = Just (r xs) where
  r [(x,_)] = Certainly x
  r xs = let (xs,ys,xn,yn) = split' xs in Odds (r xs) (xn%yn) (r ys) where
    split' []                     = ([],[],0,0)
    split' [x@(_,xn)]             = ([x],[],xn,0)
    split' (x@(_,xn):y@(_,yn):xs) = 
      let (xxs,yys,xxn,yyn) = split' xs in (x:xxs,y:yys,xxn+xn,yyn+yn)

flatten :: Odds (Odds a) -> Odds a
flatten = foldOdds Odds id

instance Applicative Odds where
  pure = Certainly
  fs <*> xs = flatten (fmap (<$> xs) fs)
  
instance Monad Odds where
  x >>= f = flatten (f <$> x)
  