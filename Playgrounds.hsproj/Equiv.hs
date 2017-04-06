{-# LANGUAGE GADTs #-}

module Equiv where
  
import Data.Semiring
import Data.Foldable.Tricks (partitionWith)
import Control.Applicative

data Equiv a where
  NatE :: Int -> Equiv Int
  TrivE :: Equiv t
  SumE :: Equiv t1 -> Equiv t2 -> Equiv (Either t1 t2) 
  ProdE :: Equiv t1 -> Equiv t2 -> Equiv (t1, t2)
  MapE ::(t1->t2)->Equiv t2->Equiv t1
  BagE :: Equiv t -> Equiv [t]
  SetE :: Equiv t -> Equiv [t]

eq:: Semiring s => Equiv t -> t -> t -> s
eq (NatE n) x y= if 0 <= x && x <= n && 0 <= y && y <= n then (if (x == y) then one else zero) else error "Argument out of range"
eq TrivE _ _ = one 
eq (SumE e1 _)(Left x)(Left y)=eq e1 x y
eq (SumE _ _) (Left _) (Right _) = zero
eq (SumE _ _) (Right _) (Left _) = zero
eq (SumE _ e2)(Right x)(Right y) = eq e2 x y
eq (ProdE e1 e2) (x1, x2) (y1, y2) =
  eq e1 x1 y1 <.> eq e2 x2 y2
eq(MapE f e) x y = eq e (f x) (f y)
eq (BagE _)[][] =one 
eq(BagE _)[](_:_) =zero
eq(BagE e)(x:xs')ys=
  case delete e x ys of
    Just ys' -> eq (BagE e) xs' ys'
    Nothing -> zero
  where
    delete::Equiv t -> t -> [t] -> Maybe[t]
    delete e v = subtract' []
      where subtract' _ [] = Nothing
            subtract' accum (x : xs) = if eq e x v then Just(accum++xs) else subtract' (x : accum) xs
eq(SetE e) xs ys = add (member e xs <$> ys) <.> add (member e ys <$> xs)
  where member :: Semiring s => Equiv t -> [t] -> t -> s
        member _ [] _ = zero
        member e (x:xs)v=eq e v x <+> member e xs v
       

data MSet s a where
  MSet :: [(a,s)] -> MSet s a
  U :: MSet s a -> MSet s a -> MSet s a
  X :: MSet s a -> MSet s b -> MSet s (a,b)

count :: Semiring s => MSet s a -> s
count (MSet xs) = add (map snd xs)
count (U xs ys) = count xs <+> count ys
count (X xs ys) = count xs <.> count ys


data Pred s a where
  Pred :: (a -> s) -> Pred s a
  TT :: Pred s a
  FF :: Pred s a
  SAnd :: Pred s a -> Pred s a -> Pred s a
  PAnd :: Pred s a -> Pred s b -> Pred s (a,b)
  SOr :: Pred s a -> Pred s a -> Pred s a
  POr :: Pred s a -> Pred s b -> Pred s (a,b)
  Is :: (a -> k, b -> k) -> Equiv k -> Pred s (a,b)
  
prob :: Semiring s => Pred s a -> a -> s
prob (Pred f) x = f x
prob TT _ = one
prob FF _ = zero
prob (p1 `SAnd` p2) x = prob p1 x <.> prob p2 x
prob (p1 `PAnd` p2) (x,y) = prob p1 x <.> prob p2 y
prob (p1 `SOr` p2) x = prob p1 x <+> prob p2 x
prob (p1 `POr` p2) (x,y) = prob p1 x <+> prob p2 y
prob ((f,g) `Is` e) (x,y) = eq e (f x) (g y)

stars :: [[a]] -> [[a]]
stars xs = [] : ((++) <$> stars xs <*> xs)