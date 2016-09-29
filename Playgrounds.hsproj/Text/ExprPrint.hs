{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}

module Text.ExprPrint
  ( Side(..)
  , ShowExpr(..)
  , Operator(..)
  , showExpr
  ) where

import           Control.Arrow
import           Data.Monoid

data Side = L | R deriving Eq

data ShowExpr t e
  = Lit t
  | Unary  (Operator t) e
  | Binary (Operator t) e e
  deriving Functor

data Operator t = Operator
  { _associativity  :: Side
  , _precedence     :: Int
  , _representation :: t }

showExpr :: Monoid t
         => (t -> t)
         -> (e -> ShowExpr t e)
         -> e -> t
showExpr prns proj = rec . proj where
  rec = showAlg . fmap ((prec &&& rec) . proj)
  showAlg = \case
    Lit t                               ->                     t
    Unary  (Operator s r t) (p,x)       ->                     t <> ifPrns R s r p x
    Binary (Operator s r t) (p,x) (q,y) -> ifPrns L s r p x <> t <> ifPrns R s r q y
  ifPrns sid oa op (Just (ia,ip))
    | ip < op || ip == op && (ia /= oa || oa /= sid) = prns
  ifPrns _ _ _ _ = id
  prec = \case
    Lit _                       -> Nothing
    Unary  (Operator s r _) _   -> Just (s,r)
    Binary (Operator s r _) _ _ -> Just (s,r)
{-# INLINABLE showExpr #-}