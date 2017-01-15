{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Network.IRC.Client.Internal.Lens
-- Copyright   : (c) 2017 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : CPP, ImpredicativeTypes
--
-- Types and functions for dealing with optics without depending on
-- the lens library.
--
-- This module is NOT considered to form part of the public interface
-- of this library.
module Network.IRC.Client.Internal.Lens where

import Control.Applicative (Const(..))
import Control.Concurrent.STM (TVar, STM, atomically, readTVar, writeTVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor.Contravariant (Contravariant)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Functor.Identity (Identity(..))
import Data.Monoid (First(..))
import Data.Profunctor (Choice)


-------------------------------------------------------------------------------
-- * Internal lens synonyms

-- | See @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Lens.html#t:Lens Control.Lens.Lens.Lens>@.
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- | A @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Type.html#t:Simple Simple>@ 'Lens'.
type Lens' s a = Lens s s a a

-- | See @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Getter.html#t:Getter Control.Lens.Getter.Getter>@.
type Getter s a = forall f. (Contravariant f, Functor f) => (a -> f a) -> s -> f s

-- | See @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Getter.html#t:Getting Control.Lens.Getter.Getting>@.
type Getting r s a = (a -> Const r a) -> s -> Const r s

-- | See @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Prism.html#t:Prism Control.Lens.Prism.Prism>@.
type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

-- | A @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Type.html#t:Simple Simple>@ 'Prism'.
type Prism' s a = Prism s s a a


-------------------------------------------------------------------------------
-- * Utilities

-- | Get the value pointed to by a getter or lens.
{-# INLINE view #-}
view :: MonadReader s m => Getting a s a -> m a
view l = asks (getConst . l Const)

-- | Map the target of a lens or prism to a monoid, and combine the results.
{-# INLINE foldMapOf #-}
foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf l f = getConst . l (Const . f)

-- | Set a value in a lens.
{-# INLINE set #-}
set :: Lens s t a b -> b -> s -> t
set l b = runIdentity . l (\_ -> Identity b)

-- | Modify a value in a lens.
{-# INLINE over #-}
over :: Lens s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

-- | Read a value from a prism.
{-# INLINE preview #-}
preview :: MonadReader s m => Getting (First a) s a -> m (Maybe a)
preview l = asks (getFirst . foldMapOf l (First . Just))


-------------------------------------------------------------------------------
-- ** STM

-- | Atomically snapshot some shared state.
snapshot :: MonadIO m => Getting (TVar a) s (TVar a) -> s -> m a
snapshot l = liftIO . atomically . readTVar . view l

-- | Atomically snapshot and modify some shared state.
snapshotModify :: MonadIO m => Lens' s (TVar a) -> (a -> STM (a, b)) -> s -> m b
snapshotModify l f s = liftIO . atomically $ do
  let avar = view l s
  a <- readTVar avar
  (a', b) <- f a
  writeTVar avar a'
  pure b
