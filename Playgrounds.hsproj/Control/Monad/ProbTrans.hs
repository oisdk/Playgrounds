{-# language FlexibleContexts, DeriveFunctor, DeriveFoldable, DeriveTraversable, LambdaCase, GeneralizedNewtypeDeriving #-}

module Control.Monad.ProbTrans where
  
import Control.Monad.ListT
import Control.Monad.Writer
import Data.Monoid
import Data.Ratio

newtype Prob a = Prob
  { runProb :: ListT (Writer (Product Rational)) a 
  } deriving (Functor, Applicative, Monad)
  
