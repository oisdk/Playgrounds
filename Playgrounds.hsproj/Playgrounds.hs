import Data.List

data Expr = Const Integer
          | SymT String
          | Mult [Expr]
          | Add  [Expr]
          deriving (Show, Eq, Ord)
          
withoutConst :: Expr -> Expr
withoutConst e = case e of
  Mult xs -> compress Mult xs
  Add  xs -> compress Add  xs
  x       -> x

compress :: ([Expr] -> Expr) -> [Expr] -> Expr
compress op xs = case ys of
  []  -> Const (-1)
  [x] -> x
  xs  -> op xs
  where ys = [ withoutConst y
             | y <- xs
             , y /= Const (-1) ]

collectVars :: [Expr] -> [Expr]
collectVars = sortOn withoutConst

