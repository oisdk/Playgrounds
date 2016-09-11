{-# language GADTs, KindSignatures, DataKinds, TypeOperators, RankNTypes, TypeFamilies #-}

module Data.FuncChain where

data FuncChain a (fs :: [*]) b where
  Fn   :: (a -> b) -> FuncChain a '[] b
  (:.) :: (b -> c) -> FuncChain a xs b -> FuncChain a (b ': xs) c

infixr 5 :.

run :: FuncChain a xs b -> a -> b
run (Fn    f) x = f x
run (f :. fs) x = f (run fs x)

undo :: FuncChain a (x ': xs) b -> FuncChain a xs x
undo (_ :. fs) = fs

type family (++) (xs :: [*]) (ys :: [*]) :: [*] where
  (++) a '[] = a
  (++) '[] b = b
  (++) (a ': as) bs = a ': (as ++ bs)
  
append :: FuncChain b xs c -> FuncChain a ys b -> FuncChain a (xs ++ (b ': ys)) c
append (Fn f)    gs = f :. gs
append (f :. fs) gs = f :. (append fs gs)

