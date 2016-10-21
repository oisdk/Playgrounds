module Data.Unfold where
  
unfoldl :: (b -> Maybe (b, a)) -> b -> [a]
unfoldl f = r [] where r a = maybe a ((uncurry.flip) (r . (:a))) . f