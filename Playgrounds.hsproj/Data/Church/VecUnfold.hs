{-# LANGUAGE TypeOperators, GADTs, ExistentialQuantification #-}

module Data.Church.VecUnfold where
  
import Data.Strict.Pair
import Data.Bifunctor
import Prelude hiding (Maybe(..), maybe)
import Data.Strict.Maybe

data VecUnfold a
  = forall b. VecUnfold
  { n :: {-# UNPACK #-} !Int 
  , f :: !(b -> Maybe (a :!: b))
  , b :: !b }

instance Functor VecUnfold where
  fmap f (VecUnfold n u b) = VecUnfold n ((fmap.first) f . u) b

instance Applicative VecUnfold where
  pure x = VecUnfold 1 (>>= (\y -> Just (y :!: Nothing))) (Just x)
  VecUnfold fn fs fb <*> VecUnfold xn xs xb
    = VecUnfold (fn * xn) ys (fs fb :!: xb)
    where
      ys (Nothing :!: _) = Nothing
      ys (Just (f :!: nfb) :!: nxb) = case xs nxb of
        Nothing -> ys (fs nfb :!: xb)
        Just (x :!: nnxb) -> Just (f x :!: (Just (f :!: nfb) :!: nnxb))
