module Data.OptArg where
  
-- Quick proof-of-concept for default parameter values in haskell
-- We're going to mimic the range function in python

-- The first (and preferred) way is by making a record type

data Range a = 
  Range { start :: a
        , stop  :: a
        , step  :: a }
        
-- Then a function which takes that datatype

range :: (Enum a, Num a) => Range a -> [a]
range r = [start r, start r + step r .. stop r]

-- Then a default value

def :: Num a => Range a
def = Range 0 0 1

-- Then the function can be called like so:

x = range (def { stop = 10 } )



--class Optional1 a b r where 
--  opt1 :: (a -> b) -> a -> r
--
--instance Optional1 a b b where
--  opt1 = id
--
--instance Optional1 a b (a -> b) where
--  opt1 = const
--
--class Optional2 a b c r where 
--  opt2 :: (a -> b -> c) -> a -> b -> r
--
--instance Optional2 a b c c where
--  opt2 = id
--
--instance (Optional1 b c r) => Optional2 a b c (a -> r) where
--  opt2 f _ b = \a -> opt1 (f a) b
--
--{- Optional3, Optional4, etc defined similarly -}

