{-# language GADTs, KindSignatures, DataKinds, TypeOperators, RankNTypes, TypeFamilies #-}

module Data.Function.Chain where
  
data FuncChain (fs :: [*]) a b where
  Fn   :: (a -> b) -> FuncChain '[] a b
  (:.) :: (b -> c) -> FuncChain xs a b -> FuncChain (b ': xs) a c

infixr 5 :.

run :: FuncChain xs a b -> a -> b
run (Fn    f) x = f x
run (f :. fs) x = f (run fs x)

undo :: FuncChain (x ': xs) a b -> FuncChain xs a x
undo (_ :. fs) = fs

type family (++) (xs :: [*]) (ys :: [*]) :: [*] where
  (++) a '[] = a
  (++) '[] b = b
  (++) (a ': as) bs = a ': (as ++ bs)
  
append :: FuncChain xs b c -> FuncChain ys a b -> FuncChain (xs ++ (b ': ys)) a c
append (Fn f)    gs = f :. gs
append (f :. fs) gs = f :. (append fs gs)
