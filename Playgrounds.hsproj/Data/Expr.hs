{-# language LambdaCase #-}

module Data.Expr where
    
import Data.Ratio
import Text.ExprPrint
  
data Expr = Zero
          | Succ Expr
          | Neg Expr
          | Abs Expr
          | Sig Expr
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
          deriving (Eq, Ord)

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
  
getNum :: Expr -> Maybe Integer
getNum = \case
  Zero -> Just 0
  Succ x -> succ <$> getNum x
  _ -> Nothing

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
  
instance Show Expr where
  show = showExpr (\s -> "(" ++ s ++ ")") $ \case
    Zero -> Lit "0"
    Succ n -> case getNum n of
      Nothing -> Binary (Operator L 4 " + ") 1 n
      Just x -> Lit (show (x+1))
    Neg n -> Unary (Operator L 1 "-") n
    Abs n -> Unary (Operator L 5 "abs ") n
    Sig n -> Unary (Operator L 5 "signum ") n
    x :+: y -> Binary (Operator L 4 " + ") x y
    x :-: y -> Binary (Operator L 5 " - ") x y
    x :*: y -> Binary (Operator L 6 " * ") x y
    

