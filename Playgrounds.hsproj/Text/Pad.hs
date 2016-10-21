{-# LANGUAGE TupleSections #-}

module Text.Pad where

repC :: a -> Int -> [a] -> [a]
repC c n r = go n where
  go 0 = r
  go n = c : go (n-1)

repS :: [a] -> Int -> [a] -> [a]
repS s n r = go n where
  go 0 = r
  go n = s ++ go (n-1)

leftAlign :: Char -> Int -> String -> ShowS
leftAlign c m x xs =
  foldr f (flip (repC c) xs) x (max 0 m) where
    f _ _ 0 = xs
    f e a n = e : a (n-1)

rightAlign :: Char -> Int -> String -> ShowS
rightAlign c m x xs =
  uncurry ($) $ foldr f base x (max 0 m) where
    f _ _ 0 = (id,xs)
    f e a n = (e:) <$> a (n-1)
    base n = (repC c n,xs)
  
centreAlign :: Char -> Int -> String -> ShowS
centreAlign c m x xs =
  uncurry ($) $ foldr f base x (max 0 m) where
    f _ _ 0 = (id,xs)
    f e a n = (e:) <$> a (n-1)
    base n = (repC c l, repC c (n-l) xs) where
      l = n `div` 2

  
showTable :: (Show cellContents)
          => Int -- ^ Column width
          -> Int -- ^ Row header width
          -> Int -- ^ Number of columns
          -> [String]
          -> [(String, [cellContents])]
          -> ShowS
showTable cw rw nc ch cn
  = showChar '┌' 
  . repC '─' rw
  . repS ('┬' : replicate cw '─') nc
  . showString "┐\n│"
  . repC ' ' rw
  . flip (foldr $ \e -> ('│':) . centreAlign ' ' cw e) ch
  . showString "│\n├"
  . repC '─' rw
  . repS ('┼' : replicate cw '─') nc
  . showChar '┤'
  . flip (foldr $ \(t,c) ->
         showString "\n│" . rightAlign ' ' rw t
         . flip (foldr $ \e -> ('│':) . rightAlign ' ' cw (show e)) c
         . showChar '│') cn
  . showString "\n└"
  . repC '─' rw
  . repS ('┴' : replicate cw '─') nc
  . showChar '┘'





