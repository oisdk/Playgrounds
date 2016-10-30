{-# language LambdaCase #-}

module Numeric.Countdown where
  
import Data.Ratio
import Text.ExprPrint
import Data.Traversable.Extras (ordNub)
import Data.List (permutations, subsequences, sort, foldl1', sortOn, unfoldr)
import Control.Monad
import Data.Ord

data Expr = Num Rational
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :/: Expr
          | Expr :*: Expr
          deriving (Eq, Ord)
          

rewrite :: (Expr -> Maybe Expr) -> Expr -> Expr
rewrite f = go where
  go = \case
    x :+: y -> let r = go x :+: go y in maybe r go (f r)
    x :*: y -> let r = go x :*: go y in maybe r go (f r)
    x :-: y -> let r = go x :-: go y in maybe r go (f r)
    x :/: y -> let r = go x :/: go y in maybe r go (f r)
    r -> maybe r go (f r)
    
gatherP, gatherM :: Expr -> [Expr]
gatherP (x :+: y) = gatherP x ++ gatherP y
gatherP x = [x]
gatherM (x :*: y) = gatherM x ++ gatherM y
gatherM x = [x]

assoc :: Expr -> Expr
assoc = \case
  x@(_ :+: _) -> (foldl1' (:+:) . sort . map assoc . gatherP) x
  x@(_ :*: _) -> (foldl1' (:*:) . sort . map assoc . gatherM) x
  x :-: y -> assoc x :-: assoc y
  x :/: y -> assoc x :/: assoc y
  x -> x
          
instance Show Expr where
  show = showExpr (\s -> "(" ++ s ++ ")") $ \case
    Num x -> Lit $ if 1 == denominator x then show (numerator x) else show x
    x :+: y -> Binary (Operator L 1 " + ") x y
    x :-: y -> Binary (Operator L 2 " - ") x y
    x :*: y -> Binary (Operator L 3 " * ") x y
    x :/: y -> Binary (Operator L 4 " / ") x y
    
eval :: Expr -> Maybe Rational
eval = \case
  Num x -> Just x
  x :+: y -> (+) <$> eval x <*> eval y
  x :-: y -> (-) <$> eval x <*> eval y
  x :/: y -> let z = eval y in if z == Just 0 then Nothing else (/) <$> eval x <*> z
  x :*: y -> (*) <$> eval x <*> eval y

instance Num Expr where
  fromInteger = Num . fromInteger
  (+) = (:+:)
  (-) = (:-:)
  (*) = (:*:)
  abs x = maybe x (Num . abs) (eval x)
  signum x = maybe x (Num . signum) (eval x)
  
instance Fractional Expr where
  fromRational = Num
  (/) = (:/:)
  
iterateTo :: (a -> Maybe a) -> a -> [a]
iterateTo f = go where
  go x = x : maybe [] go (f x)

slide :: [a] -> [([a],[a])]
slide (x:xs) = iterateTo (uncurry f) ([x],xs) where
  f _ [_] = Nothing
  f ys (z:zs) = Just ((z:ys),zs)
  

configs :: [Expr] -> [Expr]
configs = go <=< permutations <=< (reverse . tail . subsequences) where
  go [x] = [x]
  go xs = ordNub
    [ op x y
    | (l,r) <- slide xs
    , x <- go l
    , y <- go r
    , op <- [(:+:),(:-:),(:*:),(:/:)] ]
  
solve :: [Integer] -> Integer -> [Expr]
solve xs x = (ordNub . map assoc . filter ((Just (fromInteger x) ==) . eval) . configs . map fromInteger) xs