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
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Functor.Contravariant (Contravariant(contramap))
import Data.Functor.Identity (Identity(..))
import Data.Monoid (First(..))
import Data.Profunctor (Choice (right'), dimap)


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

-- | See @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Fold.html#t:Fold Control.Lens.Fold.Fold>@.
type Fold s a = forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s

-------------------------------------------------------------------------------
-- * Utilities

-- | Get the value pointed to by a getter or lens, or the result of folding
-- over all the monoidal targets of a fold.
{-# INLINE view #-}
view :: MonadReader s m => Getting a s a -> m a
view l = asks (getConst . l Const)

-- | Build a getter from an arbitrary Haskell function.
{-# INLINE to #-}
to :: (s -> a) -> Getter s a
to k f = contramap k . f . k

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

-- | Read a value from a prism or a fold.
{-# INLINE preview #-}
preview :: MonadReader s m => Getting (First a) s a -> m (Maybe a)
preview l = asks (getFirst . foldMapOf l (First . Just))

-- | Access the 'fst' field of a pair (and possibly change its type).
{-# INLINE _1 #-}
_1 :: Lens (a, c) (b, c) a b
_1 = \ afb (a, c) -> (\ b -> (b, c)) <$> afb a

-- | Access the 'snd' field of a pair (and possibly change its type).
{-# INLINE _2 #-}
_2 :: Lens (c, a) (c, b) a b
_2 = \ afb (c, a) -> (\ b -> (c, b)) <$> afb a

-- | A prism for tweaking the 'Left' half of an 'Either':
{-# INLINE _Left #-}
_Left :: Prism (Either a c) (Either b c) a b
_Left = dimap (either Right (Left . Right)) (either pure $ fmap Left) . right'

-- | A prism for tweaking the 'Right' half of an 'Either':
{-# INLINE _Right #-}
_Right :: Prism (Either c a) (Either c b) a b
_Right = dimap (either (Left . Left) Right) (either pure $ fmap Right) . right'

-- | Obtain a fold that can be composed with other optics.
{-# INLINE filtered #-}
filtered :: (a -> Bool) -> Fold a a
filtered p = dimap (\x -> if p x then Right x else Left x) (either pure id) . right'


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
