{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aztecs
  ( EntityID (..),
    World (..),
    world,
    spawn,
    lookup,
    query,
  )
where

import Data.Aztecs.Archetype (AnyArchetype (..), Archetype, Empty (..), FromAnyArchetype (..), ToArchetype, ToTypeSet (..), toArchetype)
import qualified Data.Aztecs.Archetype as A
import Data.Aztecs.Entity (Entity, EntityID (..), EntityRow)
import Data.Aztecs.Row (Row (..))
import Data.Data (TypeRep, Typeable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Typeable (Proxy (..), typeOf)
import Prelude hiding (lookup)

data Archetypes as = Archetypes
  { archetypesRow :: ArchetypesRow as,
    archetypesGetters :: Map TypeRep (ArchetypesRow as -> AnyArchetype)
  }

data Record as = Record
  { recordIndex :: Int,
    recordGetter :: ArchetypesRow as -> AnyArchetype
  }

instance Show (Record as) where
  show (Record i _) = "Record " ++ show i

data ArchetypeRecord as = ArchetypeRecord
  { archetypeRecordGetter :: ArchetypesRow as -> AnyArchetype
  }

instance Show (ArchetypeRecord as) where
  show _ = "ArchetypeRecord"

newtype ArchetypesRow as = ArchetypesRow {unArchetypesRow :: Row Archetype as}

data World
  = forall as. World
      (Archetypes as)
      (Map (Set TypeRep) (ArchetypeRecord as))
      (Map EntityID (Record as))
      (EntityID)

instance Show World where
  show (World _ archRecords records eId) =
    "World "
      ++ concatMap (\(k, v) -> show k ++ " -> " ++ show v ++ "\n") (Map.toList records)
      ++ concatMap
        (\(k, v) -> show k ++ " -> " ++ show v ++ "\n")
        (Map.toList archRecords)
      ++ show eId

world :: World
world = World (Archetypes (ArchetypesRow Nil) Map.empty) Map.empty Map.empty (EntityID 0)

addArchetype :: forall a as. (Typeable a) => Archetype a -> Archetypes as -> Archetypes (a : as)
addArchetype a (Archetypes (ArchetypesRow row) gs) =
  Archetypes
    (ArchetypesRow (Cons a row))
    ( Map.insert
        (typeOf (Proxy @a))
        (\(ArchetypesRow (Cons a' _)) -> AnyArchetype a')
        (fmap (\f -> \(ArchetypesRow (Cons _ row')) -> f (ArchetypesRow row')) gs)
    )

class Spawn a where
  spawn' :: World -> World

instance Spawn (Archetype '[]) where
  spawn' w = w

instance
  forall a as.
  ( Typeable a,
    Empty (Archetype '[a]),
    ToTypeSet (Archetype '[a]),
    Spawn (Archetype as)
  ) =>
  Spawn (Archetype (a ': as))
  where
  spawn' w@(World archs archGs rs i) =
    case Map.lookup (toTypeSet (Proxy @(Archetype '[a]))) archGs of
      Just _ -> w
      Nothing ->
        spawn' @(Archetype as) $
          World
            (addArchetype (empty @(Archetype '[a])) archs)
            ( Map.insert
                (toTypeSet (Proxy @(Archetype '[a])))
                ( ArchetypeRecord
                    { archetypeRecordGetter = \(ArchetypesRow (Cons a _)) -> AnyArchetype a
                    }
                )
                ( fmap
                    ( \r ->
                        r
                          { archetypeRecordGetter = \(ArchetypesRow (Cons _ row')) ->
                              (archetypeRecordGetter r)
                                (ArchetypesRow row')
                          }
                    )
                    archGs
                )
            )
            (mapRecords rs)
            i

spawn :: forall a. (Typeable a, ToArchetype a (EntityRow a), Spawn (Archetype a), ToTypeSet (Archetype a)) => Entity a -> World -> (EntityID, World)
spawn e (World as archGs rs i) =
  ( i,
    spawn' @(Archetype a)
      ( World
          (addArchetype (toArchetype e) as)
          ( Map.insert
              (toTypeSet (Proxy @(Archetype a)))
              ( ArchetypeRecord
                  { archetypeRecordGetter = \(ArchetypesRow (Cons a _)) -> AnyArchetype a
                  }
              )
              ( fmap
                  ( \r ->
                      r
                        { archetypeRecordGetter = \(ArchetypesRow (Cons _ row')) ->
                            (archetypeRecordGetter r)
                              (ArchetypesRow row')
                        }
                  )
                  archGs
              )
          )
          ( Map.insert
              i
              ( Record
                  { recordIndex = 0,
                    recordGetter = \(ArchetypesRow (Cons a _)) -> AnyArchetype a
                  }
              )
              (mapRecords rs)
          )
          (EntityID $ unEntityID i + 1)
      )
  )

mapRecords :: Map EntityID (Record as) -> Map EntityID (Record (a ': as))
mapRecords rs =
  fmap
    ( \r ->
        r
          { recordGetter =
              \(ArchetypesRow (Cons _ row')) ->
                (recordGetter r) (ArchetypesRow row')
          }
    )
    rs

lookup :: (FromAnyArchetype (Entity as)) => EntityID -> World -> Maybe (Entity as)
lookup eId (World as _ rs _) = do
  Record i f <- Map.lookup eId rs
  let arch = f (archetypesRow as)
  fromAnyArchetype i arch

query :: forall as. (A.Query (Entity as), ToTypeSet (Archetype as)) => World -> [Entity as]
query (World as archGs _ _) =
  let res = do
        g <- Map.lookup (toTypeSet (Proxy @(Archetype as))) archGs
        return . A.query $ (archetypeRecordGetter g) (archetypesRow as)
   in fromMaybe [] res
