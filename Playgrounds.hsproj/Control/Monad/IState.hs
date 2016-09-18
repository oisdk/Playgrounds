{-# language GADTs, KindSignatures, DataKinds, TypeOperators, RankNTypes, PolyKinds, TypeFamilies #-}
{-# language RebindableSyntax #-}

module Control.Monad.IState where

import Prelude hiding (Functor(..), Applicative(..), Monad(..))
import Data.HetList

newtype RevState i (ss :: [*]) s a 
  = RevState { runRevState :: i -> (a, HetList (s ': ss)) }

runState :: i -> RevState i ss s a -> (a, s)
runState s (RevState f) =
  let (x, rs) = f s in case rs of
    (r :- _) -> (x, r)
    
evalState :: i -> RevState i ss s a -> a
evalState s r = fst (runState s r)

execState :: i -> RevState i ss s a -> s
execState s r = snd (runState s r)