{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Writer
import Data.Ratio
import Data.Functor
import Control.Applicative
import Control.Monad

newtype Probability a = Prob
  { runProb :: WriterT (Product Rational) [] a 
  } deriving (Eq, Functor, Applicative, Monad)
  
instance Alternative Probability where
  empty = Prob (WriterT [])
  x <|> y = join $ WriterT [(x,Product (1%2)),(y, Product (1%2))]
  
instance Show a => Show (Probability a) where
  show (Prob xs) = show [ (x,p) | (x, Product p) <- runWriterT xs ]