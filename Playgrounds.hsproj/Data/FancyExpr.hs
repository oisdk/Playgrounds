{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.FancyExpr where
  
import Data.Functor.Recursive

data ExprF littype r where
  LitF :: n -> ExprF n r

  -- Num
  (:+) :: Num n => r -> r -> ExprF n r
  (:-) :: Num n => r -> r -> ExprF n r
  (:*) :: Num n => r -> r -> ExprF n r
  AbsF :: Num n => r -> ExprF n r
  SigF :: Num n => r -> ExprF n r
  NegF :: Num n => r -> ExprF n r

  -- Integral
  (:÷) :: Integral n => r -> r -> ExprF n r
  (:%) :: Integral n => r -> r -> ExprF n r

  -- Fractional
  (:/) :: Fractional n => r -> r -> ExprF n r

deriving instance Functor (ExprF n)
deriving instance Foldable (ExprF n)
deriving instance Traversable (ExprF n)
deriving instance (Eq n, Eq r) => Eq (ExprF n r)
deriving instance (Ord n, Ord r) => Ord (ExprF n r)

