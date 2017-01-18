{-# language TypeFamilies #-}
{-# language DeriveFunctor #-}
{-# language LambdaCase #-}
{-# language GADTs #-}

module Data.FancyExpr where
  
import Data.Functor.Recursive

data Final 