{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Components
  ( ComponentID (..),
    Components (..),
    empty,
    insert,
    lookup,
  )
where

import Data.Data (Typeable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable (Proxy (..), TypeRep, typeOf)
import Prelude hiding (lookup)

-- | Component ID.
newtype ComponentID = ComponentID {unComponentId :: Int}
  deriving (Eq, Ord, Show)

-- | Map of component IDs.
data Components = Components
  { componentIds :: Map TypeRep ComponentID,
    nextComponentId :: ComponentID
  }
  deriving (Show)

-- | Empty @Components@.
empty :: Components
empty =
  Components
    { componentIds = Map.empty,
      nextComponentId = ComponentID 0
    }

-- | Insert a @ComponentID@ into @Components@.
insert :: forall c. (Typeable c) => Components -> (ComponentID, Components)
insert w = case Map.lookup (typeOf (Proxy @c)) (componentIds w) of
  Just cId -> (cId, w)
  Nothing ->
    let cId = nextComponentId w
     in ( cId,
          w
            { componentIds = Map.insert (typeOf (Proxy @c)) cId (componentIds w),
              nextComponentId = ComponentID (unComponentId cId + 1)
            }
        )

-- | Lookup a @ComponentID@ from @Components@.
lookup :: forall c. (Typeable c) => Components -> Maybe ComponentID
lookup w = Map.lookup (typeOf (Proxy @c)) (componentIds w)
