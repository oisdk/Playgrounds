{-# language GADTs, KindSignatures, DataKinds, TypeOperators, RankNTypes, PolyKinds, TypeFamilies #-}
{-# language RebindableSyntax #-}

module Control.Monad.IState where

import Prelude hiding (Functor(..), Applicative(..), Monad(..))
import Data.FuncChain

newtype RevState i (ss :: [*]) s a 
  = RevState { runRevState :: FuncChain i ss (a, s) }
  
runState :: RevState i ss s a -> i -> (a, s)
runState = run . runRevState

evalState :: RevState i ss s a -> i -> a
evalState rs = fst . runState rs

execState :: RevState i ss s a -> i -> s
execState rs = snd . runState rs

return :: a -> RevState s '[] s a
return x = RevState (Fn (\s -> (x, s)))
