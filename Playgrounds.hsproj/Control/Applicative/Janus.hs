{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Control.Applicative.Janus where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Functor
import Control.Arrow
import Control.Lens (itraverse)

data Janus f a = Janus
  { forwards :: f a
  , backards :: f a }

instance Functor f => Functor (Janus f) where
  fmap f (Janus x y) = Janus (fmap f x) (fmap f y)
  
instance Alternative f => Applicative (Janus f) where
  pure x = Janus (pure x) (pure x)
  Janus fs gs <*> Janus xs ys = Janus fd (ys <**> gs <|> fd) where
    fd = fs <*> xs
  
liftJ :: f a -> Janus f a
liftJ x = Janus x x
  
run :: Alternative f => Janus f a -> f a
run (Janus f b) = f <|> b

data Ins = Jump String
         | Label String
         | Pass
         deriving Show

f :: Int -> Ins -> Janus (StateT [(String,Int)] Maybe) Int
f i Pass      = liftJ (lift (Just (-1)))
f i (Label s) = liftJ (modify ((s,i):) ) *> liftJ (lift (Just (-1)))
f i (Jump  s) = liftJ (lift =<< gets (lookup s))

parse :: [Ins] -> Maybe [Int]
parse xs = evalStateT (run (itraverse f xs)) []