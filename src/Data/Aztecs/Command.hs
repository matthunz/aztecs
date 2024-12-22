{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}

module Data.Aztecs.Command
  ( Command (..),
    spawn,
    insert,
    Edit (..),
  )
where

import Control.Monad.State (StateT (..))
import qualified Control.Monad.State as S
import Control.Monad.Writer (WriterT)
import Data.Aztecs.World (Component, Entity, World)
import qualified Data.Aztecs.World as W
import Data.Dynamic (Typeable)
import Data.Proxy
import Prelude hiding (all)

data Edit where
  Spawn :: (Component c, Typeable c) => Entity -> (Proxy c) -> Edit
  Insert :: (Component c, Typeable c) => Entity -> (Proxy c) -> Edit

-- | Command to update the `World`.
newtype Command m a = Command (StateT World (WriterT [Edit] m) a)
  deriving (Functor)

instance (Monad m) => Applicative (Command m) where
  pure a = Command $ pure a
  Command f <*> Command a = Command $ f <*> a

instance (Monad m) => Monad (Command m) where
  Command a >>= f = Command $ a >>= (\a' -> case f a' of Command b -> b)

-- | Spawn a `Component` and return its `Entity`.
spawn :: (Component a, Typeable a, Monad m) => a -> Command m Entity
spawn a = Command $ do
  w <- S.get
  let (e, w') = W.spawn a w
  S.put $ w'
  return e

-- | Insert a `Component` into an `Entity`.
insert :: (Component a, Typeable a, Monad m) => Entity -> a -> Command m ()
insert e a = Command $ do
  w <- S.get
  S.put $ W.insert e a w
  return ()
