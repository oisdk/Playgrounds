{-# language LambdaCase #-}

module Data.Expr where
    
import Data.Ratio
  
data Expr = Zero
          | Succ Expr
          | Neg Expr
          | Abs Expr
          | Sig Expr
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
          deriving (Eq, Ord, Show)

eval :: Expr -> Integer
eval = \case
  Zero    -> 0
  Succ x  -> 1 + eval x
  Neg x   -> negate (eval x)
  Abs x   -> abs (eval x)
  Sig x   -> signum (eval x)
  x :+: y -> eval x + eval y
  x :-: y -> eval x - eval y
  x :*: y -> eval x * eval y
  

fromInt :: Integer -> Expr
fromInt n = if n < 0 then Neg (r (negate n)) else r n where
  r 0 = Zero
  r n = Succ (r (n-1))

instance Num Expr where
  (+) = (:+:)
  (*) = (:*:)
  negate = Neg
  abs = Abs
  signum = Sig
  (-) = (:-:)
  fromInteger = fromInt