{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Control.Applicative.Janus where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Functor
import Control.Arrow
import Control.Lens (itraverse)

data Janus f a where
  Pure :: a -> Janus f a
  Lift :: f a -> Janus f a
  App  :: Janus f (a -> b) -> Janus f a -> Janus f b
  Then :: Janus f a -> Janus f b -> Janus f b
  Map  :: (a -> b) -> Janus f a -> Janus f b
  
instance Functor (Janus f) where
  fmap = Map
  
instance Applicative (Janus f) where
  pure = Pure
  (<*>) = App
  (*>) = Then
  (<*) = flip Then
  
run :: Alternative f => Janus f a -> f a
run (Pure x) = pure x
run (Lift x) = x
run (App fs xs) =
  let f = run fs
      x = run xs
   in f <*> x <|> x <**> f
run (Then xs ys) =
  let x = run xs
      y = run ys
  in (x *> y) <|> (y <* x)
run (Map f xs) = fmap f (run xs)

data Ins = Jump String
         | Label String
         | Pass
         deriving Show

f :: Int -> Ins -> Janus (StateT [(String,Int)] Maybe) Int
f i Pass = Lift (lift (Just (-1)))
f i (Label s) = Lift (modify ((s,i):) ) *> Lift (lift (Just (-1)))
f i (Jump s) = Lift (lift =<< gets (lookup s))

parse :: [Ins] -> Maybe [Int]
parse xs = evalStateT (run (itraverse f xs)) []

