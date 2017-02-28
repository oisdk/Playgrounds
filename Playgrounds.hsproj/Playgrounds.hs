{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Semiring

import Data.Map (Map)

import qualified Data.Map as Map

newtype SemiringMap a s = SemiringMap 
  { runSemiringMap :: Map a s 
  } deriving (Foldable, Functor)
  
scale :: Semiring s => s -> SemiringMap a s -> SemiringMap a s
scale = fmap . (<.>)

instance (Ord a, Semiring s) => Monoid (SemiringMap a s) where
  mappend (SemiringMap xs) (SemiringMap ys) = SemiringMap (Map.unionWith (<+>) xs ys)
  mempty = SemiringMap mempty
  
intersection :: (Ord a, Semiring s) => SemiringMap a s -> SemiringMap a s -> SemiringMap a s
intersection (SemiringMap xs) (SemiringMap ys) = SemiringMap (Map.intersectionWith (<.>) xs ys)

type State = Int

-- Simple NFA
data Regex s i = Regex
    { numberOfStates     :: Int
    , startingStates     :: SemiringMap State s
    , transitionFunction :: i -> State -> SemiringMap State s
    , acceptingStates    :: SemiringMap State s
    }

satisfy :: Semiring s => (i -> s) -> Regex s i
satisfy predicate = Regex n as f bs
  where
    n  = 2
    as = SemiringMap (Map.singleton 0 one)
    bs = SemiringMap (Map.singleton 1 one)

    f i 0 = SemiringMap (Map.singleton 1 (predicate i))
    f _ _ = SemiringMap Map.empty

once :: Eq i => i -> Regex Bool i
once x = satisfy (== x)

dot :: Regex Bool i
dot = satisfy (\_ -> True)

match :: Semiring s => Regex s i -> [i] -> s
match (Regex _ as _ bs)  []    = add (intersection as bs)
match (Regex n (SemiringMap as) f bs) (i:is) = match (Regex n as' f bs) is
  where
    as' = Map.foldMapWithKey (\k e -> scale e (f i k))  as
      
shift :: Int -> SemiringMap State s -> SemiringMap State s
shift n (SemiringMap xs) = SemiringMap (Map.mapKeys (+n) xs)

class Semiring s => DetectableZero s where
  isZero :: s -> Bool

instance DetectableZero Bool where isZero = not

snull :: DetectableZero s => SemiringMap a s -> Bool
snull = isZero . add

instance DetectableZero s => Semiring (Regex s i) where
    -- The regular expression that never matches anything
    zero = Regex n as f bs
      where
        n  = 0
        as = mempty
        bs = mempty
        f _ _ = mempty

    -- "ε": the regular expression that matches the empty string
    one = Regex n as f bs
      where
        n  = 1
        as = SemiringMap (Map.singleton 0 one)
        bs = SemiringMap (Map.singleton 0 one)
        f _ _ = mempty

    Regex nL asL fL bsL <+> Regex nR asR fR bsR = Regex n as f bs
      where
        n  = nL + nR
        as = mappend asL (shift nL asR)
        bs = mappend bsL (shift nL bsR)
        f i s | s < nL    = fL i s
              | otherwise = shift nL (fR i (s - nL))

    Regex nL asL fL bsL <.> Regex nR asR fR bsR = Regex n as f bs
      where
        n = nL + nR

        as =
            if snull (intersection asL bsL)
            then           asL
            else mappend asL (shift nL asR)

        f i s =
            if s < nL
            then if snull (intersection r bsL)
                 then           r
                 else mappend r (shift nL asR)
            else shift nL (fR i (s - nL))
          where
            r = fL i s

        bs = shift nL bsR
        
