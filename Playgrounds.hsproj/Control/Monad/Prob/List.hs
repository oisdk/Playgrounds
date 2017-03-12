{-# LANGUAGE DeriveFunctor #-}

module Control.Monad.Prob.List where
  
import Data.Semiring

newtype Dist s a
  = Dist
  { runDist :: [(a,s)] 
  } deriving Functor
  
instance Semiring s => Applicative (Dist s) where
  pure x = Dist [(x,one)]
  Dist fs <*> Dist xs
    = Dist
    [ (f x, fp <.> xp)
    | (f,fp) <- fs
    , (x,xp) <- xs ]
    
instance Semiring s => Monad (Dist s) where
  Dist xs >>= f
    = Dist
    [ (y, xp <.> yp)
    | (x,xp) <- xs
    , (y,yp) <- runDist (f x) ]
    
