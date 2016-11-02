{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, TypeFamilies, LambdaCase, FlexibleInstances, BangPatterns #-}

module Data.BoolSimplify where
  
import Data.Functor.Recursive
import Data.Bool
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Map (Map, member, fromList, difference, size, union, elems, insert, lookup, intersectionWith, mapMaybe, filter, assocs)
import Data.Foldable hiding (foldl1)
import Data.List (subsequences)
import Data.Maybe (fromMaybe)
import Data.Safe
import Prelude hiding (lookup, filter, foldr1, foldl1)
import Data.Function.Utils
import Data.Function.Operators
import Data.Uniques
import qualified Text.ExprPrint as Print
import Control.Lens
import Data.Traversable.Extras
import Control.Applicative

infixl 3 :&:
infixl 2 :|:

data Expr a = Var a
            | Tr
            | Fa
            | Not (Expr a)
            | Expr a :&: Expr a
            | Expr a :|: Expr a
            deriving (Eq, Ord, Functor, Foldable, Traversable)
  
flatten :: Expr (Expr a) -> Expr a
flatten = cata $ \case
  V x    -> x
  Tf     -> Tr
  Ff     -> Fa
  N x    -> Not x
  x :& y -> x :&: y
  x :| y -> x :|: y
          
instance Applicative Expr where
  pure = Var
  fs <*> xs = flatten (fmap (<$> xs) fs)
  
instance Monad Expr where
  x >>= f = flatten (fmap f x)
      
data ExprF a r 
  = V a
  | Tf
  | Ff
  | N r
  | r :& r
  | r :| r
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type instance Unfix (Expr a) = ExprF a

instance Show a => Show (Expr a) where
  show = Print.showExpr (\s -> "(" ++ s ++ ")") $ \case
    Var x -> Print.Lit (show x)
    Tr    -> Print.Lit "1"
    Fa    -> Print.Lit "0"
    Not x -> Print.Unary (Print.Operator Print.L 10 "!") x
    x :&: y -> Print.Binary (Print.Operator Print.L 3 " && ") x y
    x :|: y -> Print.Binary (Print.Operator Print.L 2 " || ") x y

instance Recursive (Expr a) where
  project = \case 
    Var n -> V n
    Tr    -> Tf
    Fa    -> Ff
    Not x -> N x
    x :&: y -> x :& y
    x :|: y -> x :| y
    
instance Corecursive (Expr a) where
  embed = \case 
    V n    -> Var n
    Tf     -> Tr
    Ff     -> Fa
    N x    -> Not x
    x :& y -> x :&: y
    x :| y -> x :|: y
  

instance Alternative Expr where
  empty = Fa
  (<|>) = (:|:)

  
eval :: (a -> Bool) -> Expr a -> Bool
eval f = cata $ \case
  V x -> f x
  Tf  -> True
  Ff  -> False
  N x -> not x
  x :& y -> x && y
  x :| y -> x || y
    
sureLookup :: Ord k => k -> Map k a -> a
sureLookup k m = let Just v = lookup k m in v

data CombiningBool = T | F | D deriving (Eq, Ord, Show)

fromBool :: Bool -> CombiningBool
fromBool True  = T
fromBool False = F

states :: Ord a => Expr a -> [Map a Bool]
states = foldlM f mempty . ordNub where
  f a e = [insert e False a, insert e True a]

minTerms :: Ord a => Expr a -> Set (Map a Bool)
minTerms e = Set.fromList [ s | s <- states e, eval (`sureLookup` s) e ]

tryCombine :: Ord a
           => Map a CombiningBool
           -> Map a CombiningBool
           -> Maybe (Map a CombiningBool)
tryCombine xs ys
  | nd == 1 = Just (fmap (uncurry cm) md)
  | otherwise = Nothing where
  md = intersectionWith (,) xs ys
  nd = size (filter (uncurry (/=)) md)
  cm x y | x == y = x
         | otherwise = D

matchUp :: Ord a => Set (Map a CombiningBool) -> Set (Map a CombiningBool)
matchUp = converge (Set.fromList . foldr f []) where
  f x [] = [x]
  f x (y:ys) = maybe (y : f x ys) (:ys) (tryCombine x y)

primes :: Ord a => Expr a -> Set (Map a Bool)
primes = minTerms 
     >>> Set.map (fmap fromBool)
     >>> matchUp
     >>> Set.map (mapMaybe f)
     where
  f T = Just True
  f F = Just False
  f D = Nothing

toExpr :: Set (Map a Bool) -> Expr a
toExpr = Set.elems
     >>> map fromMinTerm
     >>> foldl1 (:|:)
     >>> fromMaybe Fa
     where
  fromMinTerm = assocs
            >>> (map.uncurry) (bool <*> Not <<< Var)
            >>> foldl1 (:&:)
            >>> fromMaybe Tr

simplify :: Ord a => Expr a -> Expr a
simplify = toExpr . primes

instance Plated (Expr a) where
  plate f e = embed <$> traverse f (project e)
  



