{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aztecs.Entity where

import Control.Monad.Identity (Identity (..))
import Data.Aztecs.Row (Has (..), Row (..))
import Data.Data (Proxy (..), TypeRep, Typeable, typeOf)
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Base (Any)
import Unsafe.Coerce (unsafeCoerce)

newtype EntityID = EntityID {unEntityID :: Int}
  deriving (Eq, Ord, Show)

newtype EntityRow as = EntityRow {unEntityRow :: Row Identity as}

instance (Show (Row Identity as)) => Show (EntityRow as) where
  show (EntityRow row) = show row

instance (Has (Identity a) (Row Identity as)) => Has a (EntityRow as) where
  get e = let (Identity a) = get (unEntityRow e) in a

data Entity as = Entity
  { entityRow :: EntityRow as,
    getters :: Map TypeRep (EntityRow as -> Any)
  }

instance (Show (EntityRow as)) => Show (Entity as) where
  show (Entity row _) = show row

entity :: forall a. (Typeable a, Has a (EntityRow '[a])) => a -> Entity '[a]
entity a =
  Entity
    { entityRow = EntityRow $ Cons (Identity a) Nil,
      getters = Map.singleton (typeOf (Proxy @a)) (\e -> unsafeCoerce (get @a e))
    }

(<&>) ::
  forall a as.
  (Typeable a, Has a (EntityRow (a ': as))) =>
  Entity as ->
  a ->
  Entity (a ': as)
(<&>) e a =
  Entity
    { entityRow = EntityRow $ Cons (Identity a) (unEntityRow $ entityRow e),
      getters =
        Map.insert
          (typeOf (Proxy @a))
          (\e' -> unsafeCoerce (get @a e'))
          (fmap (\f -> \(EntityRow (Cons _ es)) -> f (EntityRow es)) (getters e))
    }

getComponentDyn :: forall a as. (Typeable a) => Entity as -> Maybe a
getComponentDyn e = do
  f <- Map.lookup (typeOf (Proxy @a)) (getters e)
  return . unsafeCoerce $ f (entityRow e)
