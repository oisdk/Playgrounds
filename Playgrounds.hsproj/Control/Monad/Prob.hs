{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, ViewPatterns, PatternSynonyms #-}

module Control.Monad.Prob where
  
import Data.Ratio
import Data.Monoid
import Data.List
import Control.Arrow
import Data.Uniques
import Data.Foldable

pattern (:%) :: Integer -> Integer -> Rational
pattern n :% d <- (numerator &&& denominator -> (n,d)) where
  n :% d = n % d
  
data Odds a = Certain a
            | Odds a Rational (Odds a)
            deriving (Functor, Foldable, Traversable)

foldOdds :: (a -> Rational -> b -> b) -> (a -> b) -> Odds a -> b
foldOdds f b = r where
  r (Certain x) = b x
  r (Odds x p xs) = f x p (r xs)

probOfEvent :: Eq a => a -> Odds a -> Rational
probOfEvent e = foldOdds f b where
  b x = if e == x then 1 else 0
  f x n r = (if e == x then n else r) / (n + 1)

probOf :: (a -> Bool) -> Odds a -> Rational
probOf p = foldOdds f b where
  b x = if p x then 1 else 0
  f x n r = (if p x then r + n else r) / (n + 1)

fromDistrib :: Foldable f => f (a,Integer) -> Maybe (Odds a)
fromDistrib = fst . foldr f (Nothing,0) where
  f (x,p) (a,t) = (Just (o a), t + p) where
    o = maybe (Certain x) (Odds x (p % t))

equalOdds :: (Functor f, Foldable f) => f a -> Maybe (Odds a)
equalOdds = fromDistrib . fmap (flip (,) 1)
               
append :: Odds a -> Rational -> Odds a -> Odds a
append = foldOdds f Odds where
  f e r a p ys = Odds e ip (a op ys) where
    ip = p * r / (p + r + 1)
    op = p / (r + 1)

flatten :: Odds (Odds a) -> Odds a
flatten = foldOdds append id

instance Applicative Odds where
  pure = Certain
  fs <*> xs = flatten (fmap (<$> xs) fs)
  
instance Monad Odds where
  x >>= f = flatten (f <$> x)

instance Show a => Show (Odds a) where
  showsPrec _ = foldOdds f shows where
    f x (n :% d) = flip (foldr (.))
      [ shows x, showString " |", shows n, showChar ':', shows d, showString "| " ]

compress :: Ord a => Odds a -> Odds a
compress xs = let Just ys = (fromDistrib . counts) xs in ys

