{-# language DeriveFunctor #-}

module Control.Monad.Source where
  
import Data.Scott.List

newtype Source s a =
  Source { runSource :: List s -> (a, List s) 
         } deriving Functor

instance Applicative (Source s) where
  pure x = Source $ \s -> (x, s)
  Source f <*> Source x = Source $ \s -> let (ff,ss) = f s
                                             (xx,sss) = x ss in (ff xx, sss)
                         
evalSource :: Source s a -> List s -> a
evalSource s l = fst (runSource s l)
                 
pop :: Source s (Maybe s)
pop = Source $ \s -> l s (Nothing,nil) ((,) . Just)

zipInto :: (Traversable f, Foldable g) => (a -> Maybe b -> c) -> f a -> g b -> f c
zipInto f xs = evalSource (traverse (\x -> f x <$> pop) xs) . fromList