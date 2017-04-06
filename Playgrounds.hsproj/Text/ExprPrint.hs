{-# LANGUAGE DeriveFunctor #-}

module Text.ExprPrint (Side(..), ShowExpr(..), showExpr, showSExpr) where

import Data.Semigroup
import Data.Coerce
import Control.Arrow ((&&&))

data Side = L | R deriving (Eq, Show)

data ShowExpr t e
  = ShowLit { _repr :: t }
  | Prefix  { _repr :: t, _prec :: Int, _assoc :: Side, _child  :: e }
  | Postfix { _repr :: t, _prec :: Int, _assoc :: Side, _child  :: e }
  | Binary  { _repr :: t, _prec :: Int, _assoc :: Side, _lchild :: e
                                                      , _rchild :: e }
  deriving Functor

assoc :: ShowExpr t e -> Maybe (Int,Side)
assoc (ShowLit {}) = Nothing
assoc xs = Just (_prec xs, _assoc xs)

hylozygo
    :: Functor f
    => (f a -> a) -> (f (a, b) -> b) -> (c -> f c) -> c -> b
hylozygo palg alg coalg = snd . go where
  go = (palg . fmap fst &&& alg) . fmap go . coalg

showExprAlg :: Semigroup t
            => (t -> t)
            -> ShowExpr t (Maybe (Int,Side), t)
            -> t
showExprAlg prns = go where
  go (ShowLit t)                 =                     t
  go (Prefix  t i a       (q,y)) =                     t <> ifPrns R i a q y
  go (Postfix t i a (p,x))       = ifPrns L i a p x <> t
  go (Binary  t i a (p,x) (q,y)) = ifPrns L i a p x <> t <> ifPrns R i a q y
  ifPrns sid op oa (Just (ip,ia))
    | ip < op || ip == op && (ia /= oa || sid /= oa) = prns
  ifPrns _ _ _ _ = id
  
showExpr :: Semigroup t
         => (t -> t)
         -> (e -> ShowExpr t e)
         -> e -> t
showExpr = hylozygo assoc . showExprAlg

showSExpr :: (e -> ShowExpr ShowS e) -> e -> ShowS
showSExpr coalg 
  = appEndo 
  . showExpr 
    (\e -> Endo ('(':) <> e <> Endo (')':) ) 
    (coerce coalg)