{-# language LambdaCase #-}

module Data.Expr where

data Peano = Zero | Succ Peano

data Expr = Lit Peano
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
          | Expr :^: Expr
          | Expr :/: Expr
          | Neg Expr
          | Abs Expr
          | Sig Expr