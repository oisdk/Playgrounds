{-# language GeneralizedNewtypeDeriving #-}

module Control.Monad.Undo
  ( State
  , undo
  , state
  , get
  , put
  , gets
  , modify
  , runState
  , evalState
  , execState
  , history
  ) where
  
import Data.NonEmpty
import Prelude hiding (head)
import qualified Control.Monad.State as State

tail' :: NonEmpty a -> NonEmpty a
tail' (_ :> xs) = xs
tail' x = x

newtype State s a =
  State { _runState :: State.State (NonEmpty s) a
       } deriving (Functor, Applicative, Monad)

undo :: State s ()
undo = State (State.modify tail')

get :: State s s
get = State (State.gets head)

put :: s -> State s ()
put s = State (State.modify (s:>))

gets :: (s -> a) -> State s a
gets f = fmap f get

modify :: (s -> s) -> State s ()
modify f = State (State.modify $ \xs -> f (head xs) :> xs)

runState :: State s a -> s -> (a, s)
runState (State st) = fmap head . State.runState st . Single

evalState :: State s a -> s -> a
evalState (State st) = State.evalState st . Single

execState :: State s a -> s -> s
execState (State st) = head . State.execState st . Single

history :: State s [s]
history = State (State.gets (foldr (:) []))

state :: (s -> (a, s)) -> State s a
state f = (State . State.state) $ \ss -> 
  let (x, s) = f (head ss) in
      (x, s :> ss)