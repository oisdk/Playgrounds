{-# language GADTs, KindSignatures, DataKinds, TypeOperators, RankNTypes, TypeFamilies #-}

module Data.HetList where
  
data HetList (xs :: [*]) where
  Nil  :: HetList '[]
  (:-) :: x -> HetList xs -> HetList (x ': xs)
  
type family Last (xs :: [*]) where
  Last (x ': '[]) = x
  Last (x ':  xs) = Last xs