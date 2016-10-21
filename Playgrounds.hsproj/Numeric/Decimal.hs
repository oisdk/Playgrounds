{-# LANGUAGE LambdaCase #-}

module Numeric.Decimal where
  
import Numeric.Bases

data Digit = Zero
           | One
           | Two
           | Three
           | Four
           | Five
           | Six
           | Seven
           | Eight
           | Nine
           deriving (Eq, Ord, Enum, Bounded)

charDig :: Digit -> Char
charDig = \case
  Zero  -> '0'
  One   -> '1'
  Two   -> '2'
  Three -> '3'
  Four  -> '4'
  Five  -> '5'
  Six   -> '6'
  Seven -> '7'
  Eight -> '8'
  Nine  -> '9'

instance Show Digit where
  show = pure . charDig
    
newtype Natural = Natural { runNat :: [Digit] }

instance Show Natural where show = map charDig . runNat
