module Data.Bool where
  
bool :: a -> a -> Bool -> a
bool t _ True  = t
bool _ f False = f

fi :: Bool -> a -> a -> a
fi True  t _ = t
fi False _ f = f