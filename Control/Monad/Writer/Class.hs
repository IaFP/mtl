{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
-- Search for UndecidableInstances to see why this is needed
#if MIN_VERSION_base(4,16,0)
{-# LANGUAGE QuantifiedConstraints, DefaultSignatures, ExplicitNamespaces #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Writer.Class
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- The MonadWriter class.
--
--      Inspired by the paper
--      /Functional Programming with Overloading and Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/pubs/springschool.html>)
--          Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.Writer.Class (
    MonadWriter(..),
    listens,
    censor,
  ) where

import Control.Monad.Trans.Error as Error
import Control.Monad.Trans.Except as Except
import Control.Monad.Trans.Identity as Identity
import Control.Monad.Trans.Maybe as Maybe
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (
        RWST, writer, tell, listen, pass)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (
        RWST, writer, tell, listen, pass)
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy (
        WriterT, writer, tell, listen, pass)
import qualified Control.Monad.Trans.Writer.Strict as Strict (
        WriterT, writer, tell, listen, pass)

import Control.Monad.Trans.Class (lift)
import Control.Monad
import Data.Monoid
#if MIN_VERSION_base(4,16,0)
import GHC.Types (type (@), Total)
#endif
-- ---------------------------------------------------------------------------
-- MonadWriter class
--
-- tell is like tell on the MUD's it shouts to monad
-- what you want to be heard. The monad carries this 'packet'
-- upwards, merging it if needed (hence the Monoid requirement).
--
-- listen listens to a monad acting, and returns what the monad "said".
--
-- pass lets you provide a writer transformer which changes internals of
-- the written object.

class (Monoid w, Monad m) => MonadWriter w m | m -> w where
#if __GLASGOW_HASKELL__ >= 707
    {-# MINIMAL (writer | tell), listen, pass #-}
#endif
    -- | @'writer' (a,w)@ embeds a simple writer action.
    writer :: (a,w) -> m a
#if MIN_VERSION_base(4,16,0)
    default writer :: (m @ ()) => (a,w) -> m a
#endif
    writer ~(a, w) = do
      tell w
      return a

    -- | @'tell' w@ is an action that produces the output @w@.
    tell   :: w -> m ()
    tell w = writer ((),w)

    -- | @'listen' m@ is an action that executes the action @m@ and adds
    -- its output to the value of the computation.
    listen :: m a -> m (a, w)
    -- | @'pass' m@ is an action that executes the action @m@, which
    -- returns a value and a function, and returns the value, applying
    -- the function to the output.
    pass   :: m (a, w -> w) -> m a

-- | @'listens' f m@ is an action that executes the action @m@ and adds
-- the result of applying @f@ to the output to the value of the computation.
--
-- * @'listens' f m = 'liftM' (id *** f) ('listen' m)@
listens :: (
#if MIN_VERSION_base(4,16,0)
           m @ (a, w),
#endif
           MonadWriter w m) => (w -> b) -> m a -> m (a, b)
listens f m = do
    ~(a, w) <- listen m
    return (a, f w)

-- | @'censor' f m@ is an action that executes the action @m@ and
-- applies the function @f@ to its output, leaving the return value
-- unchanged.
--
-- * @'censor' f m = 'pass' ('liftM' (\\x -> (x,f)) m)@
censor :: (
#if MIN_VERSION_base(4,16,0)
           m @ (a, w -> w), 
#endif
          MonadWriter w m) => (w -> w) -> m a -> m a
censor f m = pass $ do
    a <- m
    return (a, f)

#if MIN_VERSION_base(4,9,0)
-- | __NOTE__: This instance is only defined for @base >= 4.9.0@.
--
-- @since 2.2.2
instance (Monoid w) => MonadWriter w ((,) w) where
  writer ~(a, w) = (w, a)
  tell w = (w, ())
  listen ~(w, a) = (w, (a, w))
  pass ~(w, (a, f)) = (f w, a)
#endif

instance (
#if MIN_VERSION_base(4,16,0)
       Total m,
#endif
       Monoid w, Monad m) => MonadWriter w (Lazy.WriterT w m) where
    writer = Lazy.writer
    tell   = Lazy.tell
    listen = Lazy.listen
    pass   = Lazy.pass

instance (
#if MIN_VERSION_base(4,16,0)
       Total m,
#endif
       Monoid w, Monad m) => MonadWriter w (Strict.WriterT w m) where
    writer = Strict.writer
    tell   = Strict.tell
    listen = Strict.listen
    pass   = Strict.pass

instance (
#if MIN_VERSION_base(4,16,0)
       Total m,
#endif
       Monoid w, Monad m) => MonadWriter w (LazyRWS.RWST r w s m) where
    writer = LazyRWS.writer
    tell   = LazyRWS.tell
    listen = LazyRWS.listen
    pass   = LazyRWS.pass

instance (
#if MIN_VERSION_base(4,16,0)
       Total m,
#endif
       Monoid w, Monad m) => MonadWriter w (StrictRWS.RWST r w s m) where
    writer = StrictRWS.writer
    tell   = StrictRWS.tell
    listen = StrictRWS.listen
    pass   = StrictRWS.pass

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers
--
-- All of these instances need UndecidableInstances,
-- because they do not satisfy the coverage condition.

instance (
#if MIN_VERSION_base(4,16,0)
       Total m,
#endif
       Error e, MonadWriter w m) => MonadWriter w (ErrorT e m) where
    writer = lift . writer
    tell   = lift . tell
    listen = Error.liftListen listen
    pass   = Error.liftPass pass

-- | @since 2.2
instance (
#if MIN_VERSION_base(4,16,0)
       Total m,
#endif
       MonadWriter w m) => MonadWriter w (ExceptT e m) where
    writer = lift . writer
    tell   = lift . tell
    listen = Except.liftListen listen
    pass   = Except.liftPass pass

instance (
#if MIN_VERSION_base(4,16,0)
       Total m,
#endif
       MonadWriter w m) => MonadWriter w (IdentityT m) where
    writer = lift . writer
    tell   = lift . tell
    listen = Identity.mapIdentityT listen
    pass   = Identity.mapIdentityT pass

instance (
#if MIN_VERSION_base(4,16,0)
       Total m,
#endif
       MonadWriter w m) => MonadWriter w (MaybeT m) where
    writer = lift . writer
    tell   = lift . tell
    listen = Maybe.liftListen listen
    pass   = Maybe.liftPass pass

instance (
#if MIN_VERSION_base(4,16,0)
       Total m,
#endif
       MonadWriter w m) => MonadWriter w (ReaderT r m) where
    writer = lift . writer
    tell   = lift . tell
    listen = mapReaderT listen
    pass   = mapReaderT pass

instance (
#if MIN_VERSION_base(4,16,0)
       Total m,
#endif
       MonadWriter w m) => MonadWriter w (Lazy.StateT s m) where
    writer = lift . writer
    tell   = lift . tell
    listen = Lazy.liftListen listen
    pass   = Lazy.liftPass pass

instance (
#if MIN_VERSION_base(4,16,0)
       Total m,
#endif
       MonadWriter w m) => MonadWriter w (Strict.StateT s m) where
    writer = lift . writer
    tell   = lift . tell
    listen = Strict.liftListen listen
    pass   = Strict.liftPass pass
