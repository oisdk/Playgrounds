{-# LANGUAGE GADTs, RankNTypes #-}

{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, TypeFamilies, ScopedTypeVariables #-}

module Control.Monad.Prob where

import Data.Semiring
import Control.Parallel


newtype Prob s a b = Prob { runProb :: (b -> s) -> (a -> s) }

