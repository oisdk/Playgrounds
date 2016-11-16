{-# LANGUAGE RecursiveDo #-}

module Control.Applicative.Janus where

import Control.Monad.State.Lazy

itraverse :: Applicative f => (Int -> a -> f b) -> [a] -> f [b]
itraverse f = traverse (uncurry f) . zip [0..]

data Ins = Jump String
         | Label String
         | Pass
         deriving Show

data AssIns = Jmp Int
            | Pss
            deriving Show

parseIns :: Int -> Ins -> State [(String,Int)] AssIns
parseIns _ (Jump lbl) = do
  Just i <- gets (lookup lbl)
  return (Jmp i)
parseIns _ Pass = pure Pss
parseIns i (Label lbl) = do
  modify ((lbl,i):)
  return Pss
  
instructions :: [Ins] -> [AssIns]
instructions xs = fst (fix (runState (itraverse parseIns xs) . snd))