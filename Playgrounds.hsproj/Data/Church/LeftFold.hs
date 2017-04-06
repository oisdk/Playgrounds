{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE BangPatterns #-}

module Data.Church.LeftFold where
  
newtype LeftFold a
  = LeftFold (forall b. (b -> a -> b) -> b -> b)

instance Prelude.Functor LeftFold where
    fmap f (LeftFold xs) = LeftFold (\c -> xs (\ !a e -> c a (f e)))
    {-# INLINE fmap #-}

instance Prelude.Applicative LeftFold where
    pure x =
        LeftFold (\c b -> c b x)
    {-# INLINE pure #-}
    LeftFold fs <*> LeftFold xs =
      LeftFold (\c -> fs (\ !fb f -> xs (\ !xb x -> c xb (f x)) fb))
    {-# INLINE (<*>) #-}
    
