{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, TypeFamilies, LambdaCase, FlexibleInstances, BangPatterns #-}

module Data.BoolSimplify where
  
import Data.Functor.Recursive
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Map (Map, member, fromList, difference, size, union, elems, insert, lookup, intersectionWith, mapMaybe, filter, assocs, empty)
import Data.Foldable hiding (foldr1)
import Data.List (subsequences)
import Data.Maybe (fromMaybe)
import Data.Safe
import Prelude hiding (lookup, filter, foldr1)
import Data.Converge
import Data.Uniques
import qualified Text.ExprPrint as Print

infixl 3 :&:
infixl 2 :|:

data Expr a = Var a
            | Lit Bool
            | Not (Expr a)
            | Expr a :&: Expr a
            | Expr a :|: Expr a
            deriving (Eq, Ord, Functor, Foldable, Traversable)
      
data instance Unfix (Expr a) r 
  = V a
  | L Bool
  | N r
  | r :& r
  | r :| r
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Show a => Show (Expr a) where
  show = Print.showExpr (\s -> "(" ++ s ++ ")") $ \case
    Var x -> Print.Lit (show x)
    Lit b -> Print.Lit (show b)
    Not x -> Print.Unary (Print.Operator Print.L 10 "!") x
    x :&: y -> Print.Binary (Print.Operator Print.L 3 " && ") x y
    x :|: y -> Print.Binary (Print.Operator Print.L 2 " || ") x y

instance Recursive (Expr a) where
  project = \case 
    Var n -> V n
    Lit b -> L b
    Not x -> N x
    x :&: y -> x :& y
    x :|: y -> x :| y
  
eval :: (a -> Bool) -> Expr a -> Bool
eval f = cata $ \case
  V x -> f x
  L b -> b
  N x -> not x
  x :& y -> x && y
  x :| y -> x || y
    
sureLookup :: Ord k => k -> Map k a -> a
sureLookup k m = let Just v = lookup k m in v

data CombiningBool = T | F | D deriving (Eq, Ord, Show)

fromBool :: Bool -> CombiningBool
fromBool True  = T
fromBool False = F

states :: (Ord a, Foldable f) => f a -> [Map a Bool]
states = foldlM f empty . uniques where f a e = [ insert e False a, insert e True a ]

minTerms :: Ord a => Expr a -> Set (Map a Bool)
minTerms e = Set.fromList [ s | s <- states e, eval (`sureLookup` s) e ]

tryCombine :: Ord a => Map a CombiningBool -> Map a CombiningBool -> Maybe (Map a CombiningBool)
tryCombine xs ys | nd == 1 = Just (fmap (uncurry cm) md)
                 | otherwise = Nothing where
  md = intersectionWith (,) xs ys
  nd = size (filter (uncurry (/=)) md)
  cm x y | x == y = x
         | otherwise = D

matchUp :: Ord a => Set (Map a CombiningBool) -> Set (Map a CombiningBool)
matchUp = converge r where
  r = Set.fromList . foldr f [] where
    f x [] = [x]
    f x (y:ys) = case tryCombine x y of
      Nothing -> y : f x ys
      Just z -> z : ys

primes :: Ord a => Expr a -> Set (Map a Bool)
primes = Set.map (mapMaybe f) 
       . matchUp
       . Set.map (fmap fromBool)
       . minTerms where
  f T = Just True
  f F = Just False
  f D = Nothing
         
toExpr :: Set (Map a Bool) -> Expr a
toExpr = fromMaybe (Lit False)
       . foldr1 (:|:)
       . map ( fromMaybe (Lit True) 
             . foldr1 (:&:)
             . map (\(v,b) -> if b then Var v else Not (Var v))
             . assocs )
       . Set.elems

simplify :: Ord a => Expr a -> Expr a
simplify = toExpr . primes
