{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Aztecs.Access where

import Control.Monad (void)
import Control.Monad.State (MonadState (..), StateT (..))
import Data.Aztecs (EntityID)
import Data.Aztecs.Entity (Insertable)
import qualified Data.Aztecs.Entity as E
import Data.Aztecs.World (World)

newtype Access m a = Access (StateT World m a)
  deriving (Functor, Applicative, Monad)

runAccess :: Access m a -> World -> m (a, World)
runAccess (Access s) = runStateT s

spawn :: (Monad m, Insertable a) => a -> Access m EntityID
spawn c = Access $ do
  w <- get
  let (e, w') = E.spawn c w
  put w'
  return e

spawn_ :: (Monad m, Insertable a) => a -> Access m ()
spawn_ = void . spawn

insert :: (Monad m, Insertable a) => EntityID -> a -> Access m ()
insert e c = Access $ do
  w <- get
  put $ E.insert e c w
