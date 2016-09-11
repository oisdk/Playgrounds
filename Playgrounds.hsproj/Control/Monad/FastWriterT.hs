{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.FastWriterT
  ( WriterT
  , writerT
  , runWriterT
  , execWriterT
  , evalWriterT

  , Writer
  , writer
  , runWriter
  , execWriter
  , evalWriter

  , tell
  ) where

import Control.Monad.Trans.State.Strict
import Control.Monad.Signatures
import Control.Monad.Identity
import Control.Monad.Trans.Class

newtype WriterT w m a = WriterT_
  { rw :: StateT w m a 
  } deriving (Functor, Applicative, Monad, MonadTrans, MonadFix)

type Writer w = WriterT w Identity

instance (Monoid w, Foldable m, Monad m) => Foldable (WriterT w m) where
  foldr f b = foldr f b . evalWriterT
  foldMap f = foldMap f . evalWriterT

instance (Monoid w, Traversable m, Monad m) => Traversable (WriterT w m) where
  traverse f = fmap writerT . traverse (\(x,y) -> flip (,) y <$> f x) . runWriterT

runWriterT :: Monoid w => WriterT w m a -> m (a, w)
runWriterT (WriterT_ x) = runStateT x mempty

execWriterT :: (Monoid w, Monad m) => WriterT w m a -> m w
execWriterT (WriterT_ x) = execStateT x mempty

evalWriterT :: (Monoid w, Monad m) => WriterT w m a -> m a
evalWriterT (WriterT_ x) = evalStateT x mempty

writer :: (Monoid w, Monad m) => (a, w) -> WriterT w m a
writer (x,w) = (WriterT_ . state) (\s -> (x, mappend s w))

writerT :: (Monoid w, Monad m) => m (a, w) -> WriterT w m a
writerT m = (WriterT_ . StateT) (\s -> (fmap.fmap) (mappend s) m)

tell :: (Monoid w, Monad m) => w -> WriterT w m ()
tell = WriterT_ . modify' . mappend

runWriter :: Monoid w => Writer w a -> (a, w)
runWriter = runIdentity . runWriterT

execWriter :: Monoid w => Writer w a -> w
execWriter = runIdentity . execWriterT

evalWriter :: Monoid w => Writer w a -> a
evalWriter = runIdentity . evalWriterT 