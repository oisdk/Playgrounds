{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses #-}

module Test.Function where

import Data.Semiring

maxAndCount :: Ord a => [a] -> Maybe (Int, a)
maxAndCount