{-# language LambdaCase #-}

module Cards where

import Control.Arrow

data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades
  deriving (Eq, Ord, Enum, Bounded)
  
instance Show Suit where
  showsPrec _ = showChar . \case
    Clubs    -> '♣'
    Diamonds -> '♦'
    Hearts   -> '♥'
    Spades   -> '♠'
   
data Rank
  = Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  deriving (Eq, Ord, Enum, Bounded)
  
instance Show Rank where
  showsPrec _ = \case
    Ace   -> showString "Ace"
    Jack  -> showString "Jack"
    Queen -> showString "Queen"
    King  -> showString "King"
    s     -> (shows.fromEnum) s
    
data Card
  = Of
  { rank :: Rank
  , suit :: Suit
  } deriving
  (Eq, Ord, Bounded)
  
instance Show Card where
  showsPrec _ (r `Of` s) = shows r . showChar ' ' . shows s
  
instance Enum Card where
  fromEnum (r `Of` s) = fromEnum r + 13 * fromEnum s
  toEnum = (uncurry.flip) Of . (toEnum *** toEnum) . flip quotRem 13
  
