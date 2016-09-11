module Control.Monad.Loops
 ( whileA
 ) where

whileA :: Applicative m => m Bool -> m a -> m [a]
whileA cond action = res where
  res = if' <$> cond <*> ((:) <$> action <*> res) <*> pure []
  if' True  t _ = t
  if' False _ f = f