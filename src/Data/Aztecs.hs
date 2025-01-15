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
  )
where

import Data.Aztecs.Archetype (AnyArchetype (..), Archetype, ArchetypesRow (..), Empty (..), FromAnyArchetype (..), ToArchetype, toArchetype)
import Data.Aztecs.Entity (Entity, EntityID (..), EntityRow)
import Data.Aztecs.Row (Row (..))
import Data.Data (TypeRep, Typeable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
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

data ArchetypeRecord as = ArchetypeRecord
  { archetypeRecordGetter :: ArchetypesRow as -> AnyArchetype
  }

data World
  = forall as. World
      (Archetypes as)
      (Map (Set TypeRep) (ArchetypeRecord as))
      (Map EntityID (Record as))
      (EntityID)

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

class ToTypeSet a where
  toTypeSet :: Proxy a -> Set TypeRep

instance ToTypeSet (Archetype '[]) where
  toTypeSet _ = Set.empty

instance (Typeable a, ToTypeSet (Archetype as)) => ToTypeSet (Archetype (a ': as)) where
  toTypeSet _ = Set.insert (typeOf (Proxy @a)) (toTypeSet (Proxy @(Archetype as)))

class Spawn a where
  spawn' :: a -> World -> World

instance Spawn (Archetype '[]) where
  spawn' _ w = w

instance
  forall a as.
  ( Typeable a,
    Empty (Archetype '[a]),
    ToTypeSet (Archetype '[a])
  ) =>
  Spawn (Archetype (a ': as))
  where
  spawn' _ w@(World archs archGs rs i) =
    case Map.lookup (toTypeSet (Proxy @(Archetype '[a]))) archGs of
      Just _ -> w
      Nothing ->
        World
          (addArchetype (empty @(Archetype '[a])) archs)
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
          ( ( fmap
                ( \r ->
                    r
                      { recordGetter =
                          \(ArchetypesRow (Cons _ row')) ->
                            (recordGetter r) (ArchetypesRow row')
                      }
                )
                rs
            )
          )
          i

spawn :: (Typeable a, ToArchetype a (EntityRow a), Spawn (Archetype a)) => Entity a -> World -> (EntityID, World)
spawn e (World as archGs rs i) =
  ( i,
    spawn'
      (toArchetype e)
      ( World
          (addArchetype (toArchetype e) as)
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
          ( Map.insert
              i
              ( Record
                  { recordIndex = 0,
                    recordGetter = \(ArchetypesRow (Cons a _)) -> AnyArchetype a
                  }
              )
              ( fmap
                  ( \r ->
                      r
                        { recordGetter =
                            \(ArchetypesRow (Cons _ row')) -> (recordGetter r) (ArchetypesRow row')
                        }
                  )
                  rs
              )
          )
          (EntityID $ unEntityID i + 1)
      )
  )

lookup :: (FromAnyArchetype (Entity as)) => EntityID -> World -> Maybe (Entity as)
lookup eId (World as _ rs _) = do
  Record i f <- Map.lookup eId rs
  let arch = f (archetypesRow as)
  fromAnyArchetype i arch
