{-# LANGUAGE RankNTypes #-}

module Control.Monad.Church.Writer where
  
newtype Writer w a = W { rw :: forall c. (w -> a -> c) -> c }

instance Functor (Writer w) where
  fmap f (W p) = W (\c -> p (\w x -> c w (f x)))
  
instance Monoid w => Applicative (Writer w) where
  pure x = W (\c -> c mempty x)
  W f <*> W x = W (\c -> f (\w f -> x (\ww x ->  c (mappend w ww) (f x))))
  
instance Monoid w => Monad (Writer w) where
  W x >>= f = W (\c -> x (\w x -> rw (f x) (c . mappend w)))
  
runWriter :: Writer w a -> (w, a)
runWriter w = rw w (,)

evalWriter :: Writer w a -> a
evalWriter w = rw w (const id)

execWriter :: Writer w a -> w
execWriter w = rw w const

tell :: w -> Writer w ()
tell x = W (\c -> c x ())