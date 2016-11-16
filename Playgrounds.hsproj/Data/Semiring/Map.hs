{-# LANGUAGE DeriveFunctor, RankNTypes #-}

module Data.Semiring.Map where

import Control.Monad
import Control.Applicative

class Computation f where
  yield :: a -> f a
  
class Nondet n where
  failure :: n a
  choice :: n a -> n a -> n a
  
newtype CPS c a = CPS {(>>-) :: forall b.(a -> c b) -> c b }

runCPS :: Computation c => CPS c a -> c a
runCPS a = a >>- yield

instance Functor (CPS c) where
  fmap f x = x >>= (return . f)
  
instance Applicative (CPS c) where
  pure = return 
  (<*>) = ap

instance Monad (CPS c) where
  return x = CPS (\c -> c x)
  a >>= f = CPS (\c -> a >>- \x -> f x >>- c)
  
instance Computation (CPS c) where
  yield = return
  
instance Nondet n => Nondet (CPS n) where
  failure = CPS (\_ -> failure)
  choice a b = CPS (\c -> choice (a >>- c) (b >>- c))
  
newtype DiffList a = DiffList { unDiff :: [a] -> [a] }

instance Computation DiffList where
  yield x = DiffList (x:)
  
instance Nondet DiffList where
  failure = DiffList id
  choice (DiffList x) (DiffList y) = DiffList (x . y)
  
instance Nondet n => Alternative (CPS n) where
  empty = failure
  (<|>) = choice

toList :: DiffList a -> [a]
toList (DiffList xs) = xs []

backtrack :: CPS DiffList a -> [a]
backtrack = toList . runCPS

anyof :: (Foldable f, Computation n, Nondet n) => f a -> n a
anyof = foldl (\a e -> choice (yield e) a) failure

newtype Levels n a = Levels { levels :: [n a] }

runLevels :: Nondet n => Levels n a -> n a
runLevels = foldr choice failure . levels

levelSearch :: CPS (Levels DiffList) a -> [a]
levelSearch = toList . runLevels . runCPS

instance Computation n => Computation (Levels n) where
  yield x = Levels [yield x]
  
instance Nondet n => Nondet (Levels n) where
  failure = Levels []
  choice a b = Levels (failure : merge (levels a) (levels b))
  
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = choice x y :  merge xs ys

pytriple = do
  a <- anyof [1..]
  b <- anyof [succ a ..]
  c <- anyof [succ b ..]
  guard (a*a + b*b == c*c)
  return (a,b,c)




