{-# LANGUAGE GADTs, RankNTypes, MultiParamTypeClasses #-}

module Mat where
  
import Data.Profunctor
import Control.Category
import Prelude hiding ((.), id)
import Data.Profunctor.Traversing
import Data.Functor.Identity
import Control.Lens (TraversableWithIndex(..), FunctorWithIndex(..), FoldableWithIndex(..))
import Data.Profunctor.Types
import Data.Profunctor.Monad
import Data.Profunctor.Unsafe
import Data.Functor.Compose

data FreeITraversing i p a b where
  FreeITraversing :: TraversableWithIndex i f => (f y -> b) -> p x y -> (a -> f x) -> FreeITraversing i p a b

instance Profunctor (FreeITraversing i p) where
  lmap f (FreeITraversing l m r) = FreeITraversing l m (r . f)
  rmap g (FreeITraversing l m r) = FreeITraversing (g . l) m r
  dimap f g (FreeITraversing l m r) = FreeITraversing (g . l) m (r . f)
  g #. FreeITraversing l m r = FreeITraversing (g #. l) m r
  FreeITraversing l m r .# f = FreeITraversing l m (r .# f)

instance Strong (FreeITraversing i p) where
  second' (FreeITraversing l m r) = FreeITraversing (fmap l .# getCompose) m (Compose #. fmap r)

instance FunctorWithIndex () (Either a) where
  imap f = fmap (f ())
  
instance FoldableWithIndex () (Either a) where
  ifoldr f b = foldr (f ()) b

instance TraversableWithIndex () (Either a) where
  itraverse f = traverse (f ())

instance Choice (FreeITraversing i p) where
  right' (FreeITraversing l m r) = FreeITraversing (fmap l .# getCompose) m (Compose #. fmap r)



data Free p a b where
  Hom :: (a -> b) -> Free p a b
  Comp :: p x b -> Free p a x -> Free p a b

instance Profunctor p => Profunctor (Free p) where
  dimap l r (Hom f) = Hom (dimap l r f)
  dimap l r (Comp f g) = Comp (rmap r f) (lmap l g)

instance Profunctor p => Category (Free p) where
  id = Hom id
  Hom g . f = rmap g f
  Comp h g . f = Comp h (g . f)
  
type Arr i p = Free (FreeITraversing i p)

liftArr :: p a b -> Arr i p a b
liftArr f = Comp (FreeITraversing runIdentity f Identity) (Hom id)

runArr
  :: (Category q, Profunctor q)
  => (forall f x y . TraversableWithIndex i f => p x y -> q (f x) (f y))
  -> Arr i p a b
  -> q a b
runArr _ (Hom g) = rmap g id
runArr f (Comp (FreeITraversing unpack g pack) h) =
  dimap pack unpack (f g) . runArr f h