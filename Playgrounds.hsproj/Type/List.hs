{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Type.List where
  
import GHC.Exts
import Data.Proxy
import GHC.TypeLits

type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)
  
type family Head (xs :: [k]) :: k where
  Head (x ': xs) = x
  
type family Tail (xs :: [k]) :: [k] where
  Tail (x ': xs) = xs
  
x :: Proxy (Head '[1,2])
x = Proxy