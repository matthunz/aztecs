{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Aztecs.ECS.World.Components
  ( ComponentID (..),
    Components (..),
    empty,
    lookup,
    insert,
    insert',
  )
where

import Aztecs.ECS.Component (Component, ComponentID (..))
import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Typeable (Proxy (..), TypeRep, Typeable, typeOf)
import GHC.Generics (Generic)
import Prelude hiding (lookup)

-- | Component ID map.
data Components = Components
  { componentIds :: !(Map TypeRep ComponentID),
    nextComponentId :: !ComponentID
  }
  deriving (Show, Generic, NFData)

-- | Empty `Components`.
empty :: Components
empty =
  Components
    { componentIds = mempty,
      nextComponentId = ComponentID 0
    }

-- | Lookup a component ID by type.
lookup :: forall a. (Typeable a) => Components -> Maybe ComponentID
lookup cs = Map.lookup (typeOf (Proxy @a)) (componentIds cs)

-- | Insert a component ID by type, if it does not already exist.
insert :: forall a. (Component a) => Components -> (ComponentID, Components)
insert cs = case lookup @a cs of
  Just cId -> (cId, cs)
  Nothing -> insert' @a cs

-- | Insert a component ID by type.
insert' :: forall c. (Component c) => Components -> (ComponentID, Components)
insert' cs =
  let !cId = nextComponentId cs
   in ( cId,
        cs
          { componentIds = Map.insert (typeOf (Proxy @c)) cId (componentIds cs),
            nextComponentId = ComponentID (unComponentId cId + 1)
          }
      )
