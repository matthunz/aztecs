{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.World where

import Data.Aztecs.Components (ComponentID, Components)
import qualified Data.Aztecs.Components as CS
import Data.Aztecs.Entity (Component)
import Data.Aztecs.Table (ColumnID (..), RowID (..), Table)
import qualified Data.Aztecs.Table as Table
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

newtype EntityID = EntityID {unEntityId :: Int}
  deriving (Eq, Ord, Show)

-- | Archetype ID.
newtype ArchetypeID = ArchetypeID {unArchetypeId :: Int}
  deriving (Eq, Ord, Show)

-- | Set of component IDs.
newtype ComponentIDSet = ComponentIDSet {unComponentIdSet :: (Set ComponentID)}
  deriving (Eq, Ord, Show)

-- | Archetype component storage.
data Archetype = Archetype
  { archetypeIdSet :: ComponentIDSet,
    archetypeTable :: Table,
    archetypeAdd :: Map ComponentID ArchetypeID,
    archetypeRemove :: Map ComponentID ArchetypeID
  }
  deriving (Show)

data EntityRecord = EntityRecord
  { recordArchetypeId :: ArchetypeID,
    recordRowId :: RowID
  }
  deriving (Show)

data ComponentState = ComponentState
  { componentColumnIds :: (Map ArchetypeID ColumnID)
  }
  deriving (Show)

data World = World
  { components :: Components,
    archetypes :: Map ArchetypeID Archetype,
    archetypeIds :: Map ComponentIDSet ArchetypeID,
    nextArchetypeId :: ArchetypeID,
    componentStates :: Map ComponentID ComponentState,
    entities :: Map EntityID EntityRecord,
    nextEntityId :: EntityID
  }
  deriving (Show)

empty :: World
empty =
  World
    { components = CS.empty,
      archetypes = Map.empty,
      archetypeIds = Map.empty,
      nextArchetypeId = ArchetypeID 0,
      componentStates = Map.empty,
      entities = Map.empty,
      nextEntityId = EntityID 0
    }

spawn :: forall c. (Component c) => c -> World -> (EntityID, World)
spawn c w =
  let e = nextEntityId w
      (cId, components') = CS.insert @c (components w)
      idSet = ComponentIDSet (Set.singleton cId)
   in case Map.lookup idSet (archetypeIds w) of
        Just archId -> error "TODO"
        Nothing ->
          let archId = nextArchetypeId w
              nextArch =
                Archetype
                  { archetypeIdSet =idSet,
                    archetypeTable = Table.singleton c,
                    archetypeAdd = Map.empty,
                    archetypeRemove = Map.empty
                  }
           in ( e,
                w
                  { components = components',
                    componentStates =
                      Map.insert
                        cId
                        (ComponentState {componentColumnIds = Map.empty})
                        (componentStates w),
                    nextEntityId = EntityID (unEntityId e + 1),
                    archetypes =
                      Map.insert
                        archId
                        nextArch
                        (archetypes w),
                    archetypeIds = Map.insert idSet archId (archetypeIds w),
                    nextArchetypeId = ArchetypeID (unArchetypeId archId + 1)
                  }
              )

moveRecord :: Archetype -> Archetype -> EntityRecord -> World -> (Archetype, Archetype)
moveRecord arch nextArch record w =
  let f i (acc, nextAcc) =
        let cState = componentStates w Map.! i
            colId = componentColumnIds cState Map.! recordArchetypeId record
            nextColId = componentColumnIds cState Map.! recordArchetypeId record
            (c, acc') = Table.remove colId (recordRowId record) (archetypeTable acc)
            nextAcc' = Table.insert nextColId (recordRowId record) (archetypeTable nextAcc) c
         in (arch {archetypeTable = acc'}, nextArch {archetypeTable = nextAcc'})
   in foldr f (arch, nextArch) (unComponentIdSet (archetypeIdSet arch))
