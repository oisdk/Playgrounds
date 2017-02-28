{-# language LambdaCase #-}

module Data.Expr where

data Peano = Zero | Succ Peano deriving Show

data Expr = Lit Peano
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
          | Expr :^: Expr
          | Expr :/: Expr
          | Neg Expr
          | Abs Expr
          | Sig Expr
          deriving Show