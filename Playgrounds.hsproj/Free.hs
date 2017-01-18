{-# language UnicodeSyntax   #-}
{-# language RankNTypes      #-}
{-# language ConstraintKinds #-}

module Free where
  
newtype Free c
  = Free
  { runFree :: ∀ a. c a => a }