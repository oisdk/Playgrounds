{-# language GeneralizedNewtypeDeriving #-}

module Control.Monad.Undo where
  
import Prelude hiding (head)
import qualified Control.Monad.State as State
import Data.List.NonEmpty

newtype State s a =
  State { _runState :: State.State (NonEmpty s) a
       } deriving (Functor, Applicative, Monad)

tail' :: NonEmpty a -> NonEmpty a
tail' (_ :| (x:xs)) = x :| xs
tail' xs = xs

undo :: State s ()
undo = State (State.modify tail')

get :: State s s
get = State (State.gets head)

put :: s -> State s ()
put = State . State.modify . cons

gets :: (s -> a) -> State s a
gets f = fmap f get

modify :: (s -> s) -> State s ()
modify f = State (State.modify $ \xs -> f (head xs) <| xs)

runState :: State s a -> s -> (a, s)
runState (State st) = fmap head . State.runState st . pure

evalState :: State s a -> s -> a
evalState (State st) = State.evalState st . pure

execState :: State s a -> s -> s
execState (State st) = head . State.execState st . pure

history :: State s [s]
history = State (State.gets (foldr (:) []))

state :: (s -> (a, s)) -> State s a
state f = (State . State.state) $ \ss -> 
  let (x, s) = f (head ss) in
      (x, s <| ss)