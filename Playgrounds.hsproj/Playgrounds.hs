{-# LANGUAGE GADTs, PolyKinds, DataKinds, TypeOperators, TypeFamilies, UndecidableInstances, RankNTypes, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, ExistentialQuantification, FlexibleContexts, ScopedTypeVariables, RankNTypes, ConstraintKinds, DeriveFunctor #-}

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio
import Prelude hiding (Functor(..), Applicative(..), (<$>))
import qualified Prelude
import GHC.Exts


data Prob a where
  Prob :: (Enum a, Bounded a) => (a -> Double) -> Prob a
  
toDist :: Prob a -> Dist a
toDist (Prob f) = Dist [ (x, f x) | x <- [minBound..maxBound] ]


  
newtype Dist a = Dist {runDist :: [(a, Double)]} deriving Prelude.Functor

instance Prelude.Applicative Dist where
  pure x = Dist [(x,1)]
  Dist fs <*> Dist xs = Dist [(f x, fp*xp) | (f,fp) <- fs, (x,xp) <- xs ]

  
tot :: Prob a -> Double
tot (Prob f) = sum (map f [minBound..maxBound])

weight :: Prob a -> Dist a -> Dist a
weight (Prob f) (Dist xs) = Dist [(x, xp * f x) | (x,xp) <- xs ]

weightP :: (a -> Double) -> Prob a -> Prob a
weightP f (Prob x) = intersect (Prob f) (Prob x)
  
intersect :: Prob a -> Prob a -> Prob a
intersect (Prob xs) (Prob ys) = Prob (\x -> xs x * ys x)

data Free xs f a where
  Pure :: a -> Free '[] f a
  Ap :: Free xs f (b -> a) -> f b -> Free (b ': xs) f a

lift :: f a -> Free '[a] f a
lift = Ap (Pure id)

type family Func (xs :: [*]) (y :: *) :: * where
  Func '[] y = y
  Func (x ': xs) y = Func xs (x -> y)

data Elem (x :: k) (xs :: [k]) where
  Here :: Elem x (x ': xs)
  There :: Elem x xs -> Elem x (y ': xs)
  
(<$$>) :: a -> Free xs f (a -> b) -> Free xs f b
(<$$>) x = fmap ($x)

(<$>) = fmap

back :: Elem x xs -> Prob a -> Free xs Prob a -> Prob x
back Here r (Ap f x) = weightP (\b -> (sum . map snd . runDist) $ weight r (infer toDist (b <$$> f))) x

fmap :: (a -> b) -> Free xs f a -> Free xs f b
fmap f (Pure a) = Pure (f a)
fmap f (Ap x y) = Ap (fmap (f .) x) y
--
--
pure :: a -> Free '[] f a
pure = Pure

(<*>) :: Free xs f (a -> b) -> Free ys f a -> Free (xs ++ ys) f b
Pure f <*> y = fmap f y
Ap x y <*> z = Ap (flip <$> x <*> z) y

--reify :: (Num s, Ord a) => Prob s a -> Reified s a
--reify = Reified . Map.fromListWith (+) . runProb
--
--reflect :: Reified s a -> Prob s a
--reflect = Prob . Map.toList . runReified

infer :: Prelude.Applicative g => (f ~> g) -> Free xs f ~> g
infer _ (Pure x) = Prelude.pure x
infer u (Ap x y) = infer u x Prelude.<*> u y

type f ~> g = forall a. f a -> g a

hoist :: (f ~> g) -> Free xs f ~> Free xs g
hoist _ (Pure a) = Pure a
hoist f (Ap y x) = Ap (hoist f y) (f x) 

--die :: FactorGraph (Reified Rational) Integer
--die 
--  = lift (Graph die ((Reified . Map.fromList) [(1,1/6),(2,1/6),(3,1/6),(4,1/6),(5,1/6),(6,1/6)]))
--  
--infer' 
--
--lift :: f a -> Free f a
--lift = Ap (Pure id)
--
type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)
