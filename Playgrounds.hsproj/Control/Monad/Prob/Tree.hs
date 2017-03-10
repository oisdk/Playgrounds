{-# LANGUAGE BangPatterns, PatternSynonyms, ViewPatterns, DeriveFunctor, DeriveFoldable #-}
{-# OPTIONS_GHC -fno-defer-type-errors #-}

module Control.Monad.Prob.Tree where

import Data.Bool
import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.Semiring
import Data.Semiring.Numeric
import System.Random
import Data.Ratio
import Control.Arrow ((&&&))
import Control.Monad.State

pattern (:%:) :: () => Integral a => a -> a -> Ratio a
pattern x :%: y <- (numerator &&& denominator -> (x,y)) where
  x :%: y = x % y

data Odds o a = Certain a
              | Choice (Odds o a) (Ratio o) (Odds o a)
              deriving (Eq, Ord, Functor, Foldable, Show)
            
foldOdds :: (b -> Ratio o -> b -> b) -> (a -> b) -> Odds o a -> b
foldOdds f b = r where
  r (Certain x) = b x
  r (Choice xs p ys) = f (r xs) p (r ys)
  
unfoldOdds :: (b -> Either a (b,Ratio o,b)) -> b -> Odds o a
unfoldOdds f = r where
  r b = case f b of
    Left a -> Certain a
    Right (x,p,y) -> Choice (r x) p (r y)

probOf :: (Eq a, Semiring o, Integral o) => a -> Odds o a -> Ratio o
probOf e = foldOdds f b where
  b x = bool one zero (e == x) :%: one
  f (xn:%:xd) (n:%:d) (yn:%:yd) = ((xn<.>yd<.>n) <+> (xd<.>yn<.>d)) :%: (xd <.> yd <.> (n <+> d))

conv' :: Semiring a => Int -> a
conv' 0 = zero
conv' 1 = one
conv' n = one <+> conv' (n-1)

fromListOdds :: Integral o => (([b], Int) -> o) -> (b -> a) -> [b] -> Maybe (Odds o a)
fromListOdds fr e = r where
  r [] = Nothing
  r xs = Just (unfoldOdds f (xs, length xs))
  f ([x],_) = Left (e x)
  f (xs ,n) = Right ((ys,l), fr (ys,l) :%: fr (zs,r), (zs,r)) where
    l = n `div` 2
    r = n - l
    (ys,zs) = splitAt l xs

equalOdds :: (Semiring o, Integral o) => [a] -> Maybe (Odds o a)
equalOdds = fromListOdds (conv' . snd) id

fromDistrib :: (Semiring o, Integral o) => [(a,o)] -> Maybe (Odds o a)
fromDistrib = fromListOdds (add . map snd . fst) fst

flatten :: Odds o (Odds o a) -> Odds o a
flatten = foldOdds Choice id

instance Applicative (Odds o) where
  pure = Certain
  fs <*> xs = flatten (fmap (<$> xs) fs)

instance Monad (Odds o) where
  x >>= f = flatten (f <$> x)
  
sample :: (Integral o, Random o, Ord o, RandomGen g) => Odds o a -> g -> (a, g)
sample = runState . foldOdds (\x (n:%:d) y -> bool y x . (n>) =<< state (randomR (0,d))) pure

  


--  go (Certain x) = pure x
--  go (Choice x (n:%:d) y) =
--    go . bool y x . (n>) =<< state (randomR (0,d))
  
--lcd :: Foldable f => f Rational -> Integer
--lcd = foldl' (\a e -> lcm a (denominator e)) 1
--
--toDistrib :: Odds a -> [(a,Integer)]
--toDistrib = factorOut . foldOdds f b where
--  b x = [(x,1)]
--  f l p r = (map.fmap) (n%t*) l ++ (map.fmap) (d%t*) r where
--    n = numerator p
--    d = denominator p
--    t = n + d
--  factorOut xs = (map.fmap) (numerator . (lcd'*)) xs where
--    lcd' = fromIntegral . lcd . map snd $ xs
--
--counts :: (Ord a, Num n) => [(a,n)] -> [(a,n)]
--counts = 
--  Map.assocs . 
--  Map.fromListWith (+)
--      
--compress :: Ord a => Odds a -> Odds a
--compress xs = let Just ys = (fromDistrib . counts . toDistrib) xs in ys