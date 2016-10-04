{-# language TypeFamilies #-}
{-# language DeriveFunctor #-}
{-# language LambdaCase #-}

module Data.FancyExpr where
  
import Data.Functor.Recursive
import Data.Expr

data PeanoF r = Z | S r deriving Functor

type instance Unfix Peano = PeanoF

instance Recursive Peano where
  project Zero = Z
  project (Succ n) = S n
  
toNum :: (Num n, Enum n) => Peano -> n
toNum = cata $ \case
  Z -> 0
  S n -> succ n


--data Peano = Zero | Succ Peano
--
--data Expr = Lit Peano
--          | Expr :+: Expr
--          | Expr :-: Expr
--          | Expr :*: Expr
--          | Expr :^: Expr
--          | Expr :/: Expr
--          | Neg Expr
--          | Abs Expr
--          | Sig Expr