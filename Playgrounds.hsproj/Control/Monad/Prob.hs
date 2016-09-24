{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Control.Monad.Prob where
  
import Data.Ratio
import Data.Monoid

data Odds a = Certainly a
            | Or a Rational (Odds a)
            deriving (Functor, Foldable, Traversable)

foldOdds :: (a -> Rational -> b -> b) -> (a -> b) -> Odds a -> b
foldOdds f b = r where
  r (Certainly x) = b x
  r (Or x p xs) = f x p (r xs)
  
oddsOf :: (a -> Bool) -> Odds a -> Rational
oddsOf p = foldOdds f b where
  b x = if p x then 1 else 0
  f x n r = (if p x then r + n else r) / (n + 1)

equalOdds :: Foldable f => f a -> Odds a
equalOdds xs = foldr f undefined xs (fromIntegral $ length xs - 1) where
  f y a 0 = Certainly y
  f y a n = Or y (1 % n) (a (n-1))

instance Show a => Show (Odds a) where
  show = ('[':) . foldOdds f (\x -> show x ++ "]") where
    f x p xs = concat [show x, " (", n, ":", d, "), ", xs] where
      n = show (numerator p)
      d = show (denominator p)

conc :: Rational -> Odds a -> Odds a -> Odds a
conc p (Certainly  x) xs = Or x p xs
conc p (Or x i xs) ys = Or x ip (conc op xs ys) where
  ip = p * i / (p + i + 1)
  op = p / (i + 1)
  
flatten :: Odds (Odds a) -> Odds a
flatten (Certainly xs) = xs
flatten (Or x p xs) = conc p x (flatten xs)

instance Applicative Odds where
  pure = Certainly
  fs <*> xs = flatten (fmap (<$> xs) fs)
  
instance Monad Odds where
  x >>= f = flatten (f <$> x)