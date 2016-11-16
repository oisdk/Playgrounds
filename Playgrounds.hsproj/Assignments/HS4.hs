module Assignments.HS4 where
  
data List a
  = Nil
  | Cons a (List a)
      
instance Show a => Show (List a) where
  show Nil = "[]"
  show (Cons x xs) = (('[':) . shows x . go xs) "]" where
    go Nil = id
    go (Cons y ys) = (',':) . shows y . go ys
      
insert :: Ord a => a -> List a -> List a
insert x = go where
  go (Cons y ys) | x > y = Cons y (go ys)
  go ys = Cons x ys
  
fromList :: Ord a => [a] -> List a
fromList = foldr insert Nil

readInt :: String -> Maybe Integer
readInt = foldr (const . Just . fst) Nothing . reads

getInts :: IO [Integer]
getInts = maybe (pure []) ((<$> getInts) . (:)) . readInt =<< getLine

main :: IO ()
main = do
  putStrLn "Input some numbers"
  nums <- getInts
  putStr "Here are the numbers you input: "
  print nums
  putStr "Here is the priority list: "
  print (fromList nums)
  