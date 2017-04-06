{-# LANGUAGE FlexibleInstances, DeriveFunctor, DeriveFoldable #-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeFamilies, TypeOperators, StandaloneDeriving, ScopedTypeVariables, RankNTypes, InstanceSigs #-}
{-# LANGUAGE ImpredicativeTypes, PolyKinds, LambdaCase, MultiParamTypeClasses #-}


import Prelude hiding (sum)
import Data.Function
import Test.QuickCheck
import Data.Proxy
import Data.Type.Equality
import Numeric.Bases
import Unsafe.Coerce

data Nat = Z | S Nat
data Bit = O | I

data Tree :: Nat -> * -> * where
  Root :: a -> Node n a -> Tree n a
  
bool f t False = f
bool f t True = t

infixr 5 :<
data Node :: Nat -> * -> * where
  NilN :: Node 'Z a
  (:<) :: Tree n a -> Node n a -> Node ('S n) a

type family (-) (n :: Nat) (m :: Nat) :: Nat where
  n - 'Z = n
  'S n - 'S m = n - m

foldlNode :: (forall n. Tree n a -> b ('S n) -> b n) -> b m -> Node m a -> b 'Z
foldlNode f b NilN = b
foldlNode f b (x :< xs) = foldlNode f (f x b) xs

foldrNode :: (forall n. Tree n a -> b n -> b ('S n)) -> b 'Z -> Node n a -> b n
foldrNode f b NilN = b
foldrNode f b (x :< xs) = f x (foldrNode f b xs)

mergeTree :: Ord a => Tree n a -> Tree n a -> Tree ('S n) a
mergeTree (Root x xs) (Root y ys)
  | x <= y = Root x (Root y ys :< xs)
  | otherwise = Root y (Root x xs :< ys)

infixr 5 :-
data Heap :: Nat -> [Bit] -> * -> * where
  Nil  :: Heap n '[] a
  (:-) :: Tree z a -> Heap ('S z) xs a -> Heap z (I ': xs) a
  Zero ::             Heap ('S z) xs a -> Heap z (O ': xs) a
  
cons' :: Proxy n -> Tree z a -> Heap ('S z) (Ones n) a -> Heap z (Ones ('S n)) a
cons' _ = (:-)

type family Ones (n :: Nat) :: [Bit] where
  Ones 'Z = '[]
  Ones ('S n) = I ': Ones n

type family Drop (n :: Nat) (xs :: [Bit]) :: [Bit] where
  Drop 'Z xs = xs
  Drop ('S n) (x ': xs) = Drop n xs

type family DropOnes (n :: Nat) (m :: Nat) :: [Bit] where
  DropOnes 'Z 'Z = '[]
  DropOnes 'Z ('S n) = 'I ': DropOnes 'Z n

type family Add (ci :: Bit) (xs :: [Bit]) (ys :: [Bit]) :: [Bit] where
  Add I (I ': xs) (I ': ys) = I ': Add I xs ys
  Add I (I ': xs) (O ': ys) = O ': Add I xs ys
  Add I (O ': xs) (I ': ys) = O ': Add I xs ys
  Add I (O ': xs) (O ': ys) = I ': Add O xs ys
  Add O (I ': xs) (I ': ys) = O ': Add I xs ys
  Add O (I ': xs) (O ': ys) = I ': Add O xs ys
  Add O (O ': xs) (I ': ys) = I ': Add O xs ys
  Add O (O ': xs) (O ': ys) = O ': Add O xs ys

  Add O '[]       ys        = ys
  Add O xs        '[]       = xs

  Add I (I ': xs) '[]       = O ': Add I xs '[]
  Add I (O ': xs) '[]       = I ': xs
  Add I '[]       (I ': ys) = O ': Add I '[] ys
  Add I '[]       (O ': ys) = I ': ys
  Add I '[]       '[]       = I ': '[]
  
type family Noughts (n :: Nat) :: [Bit] where
  Noughts 'Z = '[]
  Noughts ('S n) = O ': Noughts n

type family PredAt (cin :: Bit) (n :: Nat) (xs :: [Bit]) :: [Bit] where
  PredAt c 'Z     (I ': xs) = c ': xs
  PredAt O ('S n) (O ': xs) = I ': (PredAt O n xs)
  PredAt I ('S n) (O ': xs) = O ': (PredAt I n xs)
  PredAt O ('S n) (I ': xs) = O ': (PredAt I n xs)
  PredAt I ('S n) (I ': xs) = I ': (PredAt I n xs)

type family Pred (xs :: [Bit]) :: [Bit] where
  Pred (I ': xs) = O ': xs
  Pred (O ': xs) = I ': (Pred xs)



merge :: Ord a => Heap z xs a -> Heap z ys a -> Heap z (Add O xs ys) a
merge Nil       (y :- ys) = y :- ys
merge Nil       (Zero ys) = Zero ys
merge Nil       Nil       = Nil
merge (x :- xs) Nil       = x :- xs
merge (x :- xs) (Zero ys) = x :- merge xs ys
merge (x :- xs) (y :- ys) = Zero (merge' (mergeTree x y) xs ys)
merge (Zero xs) Nil       = Zero xs
merge (Zero xs) (y :- ys) = y :- merge xs ys
merge (Zero xs) (Zero ys) = Zero (merge xs ys)

merge' :: Ord a => Tree z a -> Heap z xs a -> Heap z ys a -> Heap z (Add I xs ys) a
merge' t Nil       Nil       = t :- Nil
merge' t Nil       (y :- ys) = Zero (merge' (mergeTree t y) Nil ys)
merge' t Nil       (Zero ys) = t :- ys
merge' t (x :- xs) Nil       = Zero (merge' (mergeTree t x) xs Nil)
merge' t (x :- xs) (Zero ys) = Zero (merge' (mergeTree t x) xs ys)
merge' t (x :- xs) (y :- ys) = t :- (merge' (mergeTree x y) xs ys)
merge' t (Zero xs) Nil       = t :- xs
merge' t (Zero xs) (y :- ys) = Zero (merge' (mergeTree t y) xs ys)
merge' t (Zero xs) (Zero ys) = t :- merge xs ys

singleton :: a -> Heap 'Z '[I] a
singleton x = Root x NilN :- Nil

empty :: Heap 'Z '[] a
empty = Nil

type family (+) (n :: Nat) (m :: Nat) :: Nat where
  'Z + m = m
  ('S n) + m = 'S (n + m)
  
type family (++) (xs :: [Bit]) (ys :: [Bit]) :: [Bit] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

cong :: forall (a :: k) (b :: k) (f :: k -> t). a :~: b -> f a :~: f b
cong Refl = Refl

convolve :: Heap n xs a -> Node n a -> Heap 'Z (Ones n ++ xs) a
convolve xs ys = fst (go xs ys) where
  go :: forall n xs a. Heap n xs a -> Node n a -> (Heap 'Z (Ones n ++ xs) a, forall ys. Proxy (ys :: [Bit]) -> ('I ': (Ones n ++ ys)) :~: Ones n ++ (I ': ys))
  go h NilN = (h, (\_ -> Refl))
  go (h :: Heap n xs a) (t :< ts) = case go (t :- h) ts of
      (r,prf) -> case prf (Proxy :: Proxy xs) of
        Refl -> (r, \p -> cong (prf p))
      
minView :: Ord a => Heap 'Z xs a -> (a, Heap 'Z (Pred xs) a)
minView = go id where
  go :: Ord a => forall xs ys. (Heap 'Z (Ones z ++ xs) a -> Heap 'Z ys a) -> Heap z xs a -> (a, Heap 'Z ys a)
  go f (Root x ns :- ts) = case go (merge (Root x ns :- Nil) . Zero . f) ts of
    (y,ys) | y < x -> (y,ys)
           | otherwise -> (x, (convolve (Zero ts) ns))