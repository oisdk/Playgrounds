{-# LANGUAGE GADTs, RankNTypes #-}
{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, TypeFamilies, ScopedTypeVariables #-}

module Control.Monad.Prob where

import Data.Semiring
import Control.Parallel

infixr 5 :-

data Vect (xs :: [*]) where
  Nil  :: Vect '[]
  (:-) :: x -> Vect xs -> Vect (x ': xs)
    
class AllFinite xs where
  margVec :: Semiring n => (Vect xs -> n) -> n
  
instance (AllFinite xs, Enum x, Bounded x) => AllFinite (x ': xs) where
  margVec f = add (map (\x -> margVec (\xs -> f (x :- xs))) [minBound..maxBound])
  
instance AllFinite '[] where
  margVec f = f Nil
  
type family Curried (xs :: [*]) (a :: *) where
  Curried '[] a = a
  Curried (x ': xs) a = x -> Curried xs a
  
class Curryable xs where
  curryV :: (Vect xs -> a) -> Curried xs a

instance Curryable '[] where
  curryV f = f Nil
  
instance Curryable xs => Curryable (x ': xs) where
  curryV f = \x -> curryV (\xs -> f (x :- xs))
  
class UnCurryable xs where
  unCurryV :: Curried xs a -> Vect xs -> a
  
instance UnCurryable '[] where
  unCurryV x Nil = x
  
instance UnCurryable xs => UnCurryable (x ': xs) where
  unCurryV f (x :- xs) = unCurryV (f x) xs

data Prob s a where
  Dst :: (a -> s) -> Prob s a
  Bnd :: AllFinite xs => MultiProb s xs -> (Vect xs -> Prob s b) -> Prob s b
  
data MultiProb s xs where
  NilP :: MultiProb s '[]
  ConP :: Prob s x -> MultiProb s xs -> MultiProb s (x ': xs)
  
getProb :: Semiring s => Prob s a -> a -> s
getProb (Dst f) = f
getProb (Bnd ps f) = \y -> margVec (\ws -> allMessages ps ws <.> getProb (f ws) y) where
  
  funcs :: forall s ys. Semiring s => MultiProb s ys -> s -> Vect ys -> s
  funcs NilP = \n _ -> n
  funcs (ConP p ps) =
    let m = getProb p
        ms = m `par` funcs ps
        in \n (v :- vs) -> ms (n <.> m v) vs
        
  allMessages :: forall ys s. Semiring s => MultiProb s ys -> Vect ys -> s
  allMessages ps = funcs ps one