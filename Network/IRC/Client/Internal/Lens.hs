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
module Network.IRC.Client.Internal.Lens where

import Control.Applicative (Const(..))
import Control.Arrow (Kleisli(..))
import Control.Concurrent.STM (TVar, STM, atomically, readTVar, writeTVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Functor.Contravariant (Contravariant)
import Data.Functor.Identity (Identity(..))
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

-- | A more direct implementation of
-- @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Extras.html#v:is Control.Lens.Extras.is>@.
{-# INLINE is #-}
is :: Prism s t a b -> s -> Bool
is k = either (const True) (const False . runIdentity)
    . runKleisli (k (Kleisli Left))


-------------------------------------------------------------------------------
-- * Utilities

-- Also it'd be nice to use the standard lens names for various things to
-- make it easier on other people working on irc-client.

-- | Get a value from a lens.
get :: Getting a s a -> s -> a
get lens = getConst . lens Const
-- FIXME: this is (almost) 'Control.Lens.Getter.view' or '^.'
-- also clashes with MonadState's get/put

-- view :: Getting a s a -> s -> a -- instance MonadReader ((->) s)
view :: MonadReader s m => Getting a s a -> m a
view l = asks (getConst . l Const)

-- | Set a value in a lens.
set :: Lens' s a -> a -> s -> s
set l a = runIdentity . l (\_ -> Identity a)
-- also called '.~'
-- FIXME: arg name clashes with 'Control.Lens.Lens.lens'; usually just 'l'

-- | Modify a value in a lens.
modify :: Lens' s a -> (a -> a) -> s -> s
{- modify lens f s = let a = get lens s in set lens (f a) s -}
-- FIXME: this is 'Control.Lens.Setter.over', or '%~'
-- name also clashes with MonadState
over :: Lens' s a -> (a -> a) -> s -> s
over l f = runIdentity . l (Identity . f)
modify = over
