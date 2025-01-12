{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.World where

import Data.Aztecs (Component, EntityID (..))
import Data.Aztecs.Components (ComponentID, Components)
import qualified Data.Aztecs.Components as CS
import Data.Aztecs.Table (ColumnID (..), RowID (..), Table)
import qualified Data.Aztecs.Table as Table
import Data.Data (Proxy (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- | Archetype ID.
newtype ArchetypeID = ArchetypeID {unArchetypeId :: Int}
  deriving (Eq, Ord, Show)

-- | Set of component IDs.
newtype ComponentIDSet = ComponentIDSet {unComponentIdSet :: (Set ComponentID)}
  deriving (Eq, Ord, Show, Semigroup, Monoid)

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

data ComponentProxy = forall c. (Component c) => ComponentProxy (Proxy c)

instance Show ComponentProxy where
  show (ComponentProxy p) = show p

data ComponentState = ComponentState
  { componentColumnIds :: (Map ArchetypeID ColumnID),
    proxy :: ComponentProxy
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
        Just archId ->
          let arch = archetypes w Map.! archId
           in case Map.lookup cId (componentStates w) of
                Just cState ->
                  let colId = componentColumnIds cState Map.! archId
                      arch' = arch {archetypeTable = Table.insert colId (RowID 0) c (archetypeTable arch)}
                   in ( e,
                        w
                          { components = components',
                            componentStates =
                              Map.insert
                                cId
                                cState
                                (componentStates w),
                            entities = Map.insert e (EntityRecord {recordArchetypeId = archId, recordRowId = RowID 0}) (entities w),
                            nextEntityId = EntityID (unEntityId e + 1),
                            archetypes = Map.insert archId arch' (archetypes w)
                          }
                      )
                Nothing ->
                  let colId = ColumnID (Table.length (archetypeTable arch))
                      cState = ComponentState {componentColumnIds = Map.singleton archId colId, proxy = ComponentProxy (Proxy @c)}
                      arch' = arch {archetypeTable = Table.cons c (archetypeTable arch)}
                   in ( e,
                        w
                          { components = components',
                            componentStates =
                              Map.insert
                                cId
                                cState
                                (componentStates w),
                            entities = Map.insert e (EntityRecord {recordArchetypeId = archId, recordRowId = RowID 0}) (entities w),
                            nextEntityId = EntityID (unEntityId e + 1),
                            archetypes = Map.insert archId arch' (archetypes w)
                          }
                      )
        Nothing ->
          let nextArch =
                Archetype
                  { archetypeIdSet = idSet,
                    archetypeTable = Table.singleton c,
                    archetypeAdd = Map.empty,
                    archetypeRemove = Map.empty
                  }
              (archId, w') = insertArchetype nextArch w
           in ( e,
                w'
                  { components = components',
                    componentStates =
                      Map.insert
                        cId
                        ( ComponentState
                            { componentColumnIds = Map.singleton archId (ColumnID 0),
                              proxy = ComponentProxy (Proxy @c)
                            }
                        )
                        (componentStates w),
                    entities = Map.insert e (EntityRecord {recordArchetypeId = archId, recordRowId = RowID 0}) (entities w),
                    nextEntityId = EntityID (unEntityId e + 1)
                  }
              )

insertArchetype :: Archetype -> World -> (ArchetypeID, World)
insertArchetype arch w =
  let archId = nextArchetypeId w
   in ( archId,
        w
          { archetypes =
              Map.insert
                archId
                arch
                (archetypes w),
            archetypeIds = Map.insert (archetypeIdSet arch) archId (archetypeIds w),
            nextArchetypeId = ArchetypeID (unArchetypeId archId + 1)
          }
      )

insert :: forall c. (Component c) => EntityID -> c -> World -> World
insert e c w =
  let (cId, components') = CS.insert @c (components w)
   in case Map.lookup cId (componentStates w) of
        Just _ -> insertWithId e cId c w {components = components'}
        Nothing ->
          let record = entities w Map.! e
              arch = archetypes w Map.! recordArchetypeId record
              nextIdSet = ComponentIDSet (Set.insert cId (unComponentIdSet (archetypeIdSet arch)))
           in case Map.lookup nextIdSet (archetypeIds w) of
                Just nextArchId -> error "TODO"
                Nothing ->
                  let nextArch =
                        Archetype
                          { archetypeIdSet = nextIdSet,
                            archetypeTable = Table.singleton c,
                            archetypeAdd = Map.empty,
                            archetypeRemove = Map.empty
                          }
                      (nextArchId, w') = insertArchetype nextArch w
                   in moveArchetype
                        e
                        record
                        arch
                        nextArchId
                        nextArch
                        w'
                          { componentStates =
                              Map.insert
                                cId
                                ( ComponentState
                                    { componentColumnIds =
                                        Map.singleton nextArchId (ColumnID $ Table.length (archetypeTable nextArch)),
                                      proxy = ComponentProxy (Proxy @c)
                                    }
                                )
                                (componentStates w')
                          }

insertWithId :: forall c. (Component c) => EntityID -> ComponentID -> c -> World -> World
insertWithId e cId c w =
  let record = entities w Map.! e
      arch = archetypes w Map.! recordArchetypeId record
      nextIdSet = ComponentIDSet (Set.insert cId (unComponentIdSet (archetypeIdSet arch)))
   in case Map.lookup nextIdSet (archetypeIds w) of
        Just nextArchId -> error "TODO"
        Nothing ->
          let nextArchId = nextArchetypeId w
              nextArch =
                Archetype
                  { archetypeIdSet = nextIdSet,
                    archetypeTable = Table.singleton c,
                    archetypeAdd = Map.empty,
                    archetypeRemove = Map.empty
                  }
              w' = moveArchetype e record arch nextArchId nextArch w
           in w'
                { archetypeIds = Map.insert (archetypeIdSet arch) (recordArchetypeId record) (archetypeIds w),
                  nextArchetypeId = ArchetypeID (unArchetypeId nextArchId + 1)
                }

moveArchetype :: EntityID -> EntityRecord -> Archetype -> ArchetypeID -> Archetype -> World -> World
moveArchetype e record arch nextArchId nextArch w =
  let (arch', nextArch', w') = moveRecord arch nextArchId nextArch record w
   in w'
        { archetypes =
            Map.fromList
              [ (recordArchetypeId record, arch'),
                (nextArchId, nextArch')
              ]
              <> archetypes w,
          entities =
            Map.insert
              e
              ( EntityRecord
                  { recordArchetypeId = nextArchId,
                    recordRowId = recordRowId record
                  }
              )
              (entities w)
        }

moveRecord :: Archetype -> ArchetypeID -> Archetype -> EntityRecord -> World -> (Archetype, Archetype, World)
moveRecord arch archId nextArch record w =
  let f i (acc, nextAcc, wAcc) =
        let cState = componentStates wAcc Map.! i
         in moveComponent (proxy cState) record i cState acc archId nextAcc wAcc
   in foldr f (arch, nextArch, w) (unComponentIdSet (archetypeIdSet arch))

moveComponent ::
  ComponentProxy ->
  EntityRecord ->
  ComponentID ->
  ComponentState ->
  Archetype ->
  ArchetypeID ->
  Archetype ->
  World ->
  (Archetype, Archetype, World)
moveComponent (ComponentProxy p) = goProxy p
  where
    goProxy ::
      forall c.
      (Component c) =>
      Proxy c ->
      EntityRecord ->
      ComponentID ->
      ComponentState ->
      Archetype ->
      ArchetypeID ->
      Archetype ->
      World ->
      (Archetype, Archetype, World)
    goProxy _ = go @c

    go ::
      forall c.
      (Component c) =>
      EntityRecord ->
      ComponentID ->
      ComponentState ->
      Archetype ->
      ArchetypeID ->
      Archetype ->
      World ->
      (Archetype, Archetype, World)
    go record cId cState arch nextArchId nextArch w =
      let colId = componentColumnIds cState Map.! recordArchetypeId record
          (c, acc') = Table.remove @c colId (recordRowId record) (archetypeTable arch)
          nextAcc' = Table.cons c (archetypeTable nextArch)
       in ( arch {archetypeTable = acc'},
            nextArch {archetypeTable = nextAcc'},
            w
              { componentStates =
                  Map.insert
                    cId
                    cState
                      { componentColumnIds =
                          Map.insert
                            nextArchId
                            (ColumnID ((Table.length $ archetypeTable nextArch) - 1))
                            (componentColumnIds cState)
                      }
                    (componentStates w)
              }
          )