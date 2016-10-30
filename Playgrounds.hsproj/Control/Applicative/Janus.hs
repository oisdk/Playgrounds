{-# LANGUAGE DeriveFunctor, FlexibleContexts #-}

module Control.Applicative.Janus where

import Control.Arrow
import Data.Functor
import Data.Monoid
import Control.Monad.State
import Control.Applicative
import Data.List
import Control.Lens (itraverse)

data Janus f a = Janus
  { forwards  :: f a
  , backwards :: f a 
  } deriving (Functor,Show)
  
instance Applicative f => Applicative (Janus f) where
  pure x = Janus (pure x) (pure x)
  Janus ff fb <*> Janus xf xb = Janus (ff <*> xf) (xb <**> fb)
  
run :: Alternative f => Janus f a -> f a
run (Janus f b) = f <|> b

both :: f a -> Janus f a
both x = Janus x x

data Ins = Jump String
         | Label String
         | Pass
         deriving Show

newtype Parser a b = Parser { runParse :: Int -> [a] -> Maybe (Int,[a],b)} deriving Functor

instance Applicative (Parser a) where
  pure x = Parser $ \p xs -> Just (p,xs,x)
  fs <*> xs = Parser $ \p s -> do
    (p,s,f) <- runParse fs p s
    (p,s,x) <- runParse xs p s
    pure (p, s, f x)



parse :: [Ins] -> [Maybe Int]
parse = uncurry (zipWith (<|>)) . (forwards &&& backwards) . evalIn . itraverse f

evalIn (Janus xs ys) = Janus (evalState xs []) (evalState ys [])

f :: Int -> Ins -> Janus (State [(String,Int)]) (Maybe Int)
f i Pass = pure Nothing
f i (Label s) = both (modify ((s,i):) ) $> Nothing
f i (Jump s) = both (gets (lookup s))

