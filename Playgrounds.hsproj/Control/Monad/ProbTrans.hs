{-# language FlexibleContexts, DeriveFunctor, DeriveFoldable, DeriveTraversable, LambdaCase #-}

module Control.Monad.ProbTrans where
  
import Data.Ratio
import Data.Monoid
import Data.Bifunctor
import Data.Foldable
import Control.Monad.Identity

newtype OddsT m a = OddsT { runOddsT :: m (Either a (a,Rational,OddsT m a)) }

instance Functor m => Functor (OddsT m) where
  fmap f (OddsT x) = OddsT (fmap (bimap f (\(x,y,z) -> (f x, y, fmap f z))) x)
  
instance Monad m => Applicative (OddsT m) where
  pure = OddsT . pure . Left
  fs <*> xs = flatten (fmap (<$> xs) fs)
  
instance Monad m => Monad (OddsT m) where
  x >>= f = flatten (f <$> x)

conc :: Monad m => Rational -> OddsT m a -> OddsT m a -> OddsT m a
conc p (OddsT x) xs = OddsT $ x >>= \case
  Left y -> pure (Right (y,p,xs))
  Right (y,i,ys) -> pure (Right (y,ip,conc op ys xs)) where
    ip = p * i / (p + i + 1)
    op = p / (i + 1)

flatten :: Monad m => OddsT m (OddsT m a) -> OddsT m a
flatten (OddsT x) = OddsT $ x >>= \case
  Left (OddsT x) -> x
  Right (x,p,xs) -> runOddsT (conc p x (flatten xs))

foldOdds :: Monad m => (a -> m b) -> (a -> Rational -> b -> m b) -> OddsT m a -> m b
foldOdds b f = r where
  r (OddsT x) = x >>= \case
    Left x -> b x
    Right (x,y,z) -> f x y =<< r z

oddsOf :: Monad m => (a -> Bool) -> OddsT m a -> m Rational
oddsOf p = foldOdds b f where
  b x = pure $ if p x then 1 else 0
  f x n r = pure $ (if p x then r + n else r) / (n + 1)

equalOdds :: (Monad m, Foldable f) => f a -> OddsT m a
equalOdds xs = foldr f undefined xs (fromIntegral $ length xs - 1) where
  f y a 0 = pure y
  f y a n = OddsT . pure . Right $ (y, 1 % n,a (n-1))
  
instance Foldable f => Foldable (OddsT f) where
  foldMap f (OddsT x) = foldMap (either f (\(x,y,z) -> f x `mappend` foldMap f z)) x
  
instance Traversable f => Traversable (OddsT f) where
  traverse f (OddsT x) = 
    OddsT <$> traverse (either (fmap Left . f) 
                               (\(x,y,z) -> fmap Right $ (,,) <$> f x <*> pure y <*> traverse f z)) x