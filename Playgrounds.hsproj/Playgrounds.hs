-- | Semiring-based weighting
-- | Semiring-writer
-- | Probability
-- | Heuristic search
-- | Free applicatives
-- | Is Control

import Data.Foldable

data Three
  = One
  | Two
  | Three
  deriving (Eq, Show)
  
--instance Ord Three where
--  compare Three Three = EQ
--  compare Three _     = GT
--  compare Two   Three = LT
--  compare Two   Two   = EQ
--  compare Two   One   = GT
--  compare One   One   = EQ
--  compare One   _     = LT
--  One   <= _     = True
--  Two   <= One   = False
--  Two   <= _     = True
--  Three <= Three = True
--  Three <= _     = False
--  Three >= _     = True
--  Two   >= Three = False
--  Two   >= _     = True
--  One   >= One   = True
--  One   >= _     = False
--  
--maxSlow :: Ord a => [a] -> Maybe a
--maxSlow = foldr (max . Just) Nothing
--
--maxFast :: Ord a => [a] -> Maybe a
--maxFast = foldl (\a -> max a . Just) Nothing