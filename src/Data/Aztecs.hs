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
    Entity (..),
    entity,
    (<&>),
    getComponentDyn,
    Archetype (..),
    ToArchetype (..),
    toArchetype,
    World (..),
    world,
    spawn,
    lookup,
  )
where

import Control.Monad.Identity (Identity (Identity))
import Data.Data (TypeRep, Typeable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Proxy (..), typeOf)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Base (Any)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (lookup)

newtype EntityID = EntityID {unEntityID :: Int}
  deriving (Eq, Ord, Show)

data Row f (as :: [k]) where
  Nil :: Row f '[]
  Cons :: f a -> Row f as -> Row f (a ': as)

instance Show (Row f '[]) where
  show Nil = "[]"

instance (Show (f a), Show' (Row f as)) => Show (Row f (a ': as)) where
  show (Cons x xs) = "[ " ++ show x ++ showRow xs

class Show' a where
  showRow :: a -> String

instance Show' (Row f '[]) where
  showRow Nil = "]"

instance (Show (f a), Show' (Row f as)) => Show' (Row f (a ': as)) where
  showRow (Cons x xs) = ", " ++ show x ++ showRow xs

instance {-# OVERLAPPING #-} Has (f a) (Row f (a ': ts)) where
  get (Cons x _) = x

instance {-# OVERLAPPING #-} (Has (f a) (Row f ts)) => Has (f a) (Row f (b ': ts)) where
  get (Cons _ xs) = get xs

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

class Has a c where
  get :: c -> a

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

class FromAnyArchetype a where
  fromAnyArchetype :: Int -> AnyArchetype -> Maybe a

instance FromAnyArchetype (Entity '[]) where
  fromAnyArchetype _ _ = Nothing

instance {-# OVERLAPPING #-} (Typeable a) => FromAnyArchetype (Entity '[a]) where
  fromAnyArchetype i (AnyArchetype a) = do
    f <- Map.lookup (typeOf (Proxy @a)) (archetypeGetters a)
    e <- unsafeCoerce $ f i (archetypeRow a)
    let e' = entity (unsafeCoerce e)
        EntityRow (Cons x Nil) = entityRow e'
    return $
      ( e'
          { entityRow = EntityRow $ Cons x Nil,
            getters =
              Map.singleton
                (typeOf (Proxy @a))
                (\e'' -> unsafeCoerce (get @a e''))
          }
      )

instance (Typeable a, FromAnyArchetype (Entity as)) => FromAnyArchetype (Entity (a ': as)) where
  fromAnyArchetype i (AnyArchetype a) = do
    f <- Map.lookup (typeOf (Proxy @a)) (archetypeGetters a)
    e <- unsafeCoerce $ f i (archetypeRow a)
    let e' = entity (unsafeCoerce e)
        EntityRow (Cons x Nil) = entityRow e'
    Entity (EntityRow row') gs <- fromAnyArchetype @(Entity as) i (AnyArchetype a)
    return $
      ( e'
          { entityRow = EntityRow $ Cons x row',
            getters =
              Map.insert
                (typeOf (Proxy @a))
                (\e'' -> unsafeCoerce (get @a e''))
                (fmap (\g -> \(EntityRow (Cons _ es)) -> g (EntityRow es)) gs)
          }
      )

newtype ArchetypeRow as = ArchetypeRow {unArchetypeRow :: Row Vector as}

data Archetype as = Archetype
  { archetypeRow :: ArchetypeRow as,
    archetypeGetters :: Map TypeRep (Int -> ArchetypeRow as -> Maybe Any)
  }

class Empty a where
  empty :: a

instance Empty (Archetype '[]) where
  empty = Archetype {archetypeRow = ArchetypeRow Nil, archetypeGetters = Map.empty}

instance (Empty (Archetype as)) => Empty (Archetype (a ': as)) where
  empty =
    let next = empty
     in Archetype {archetypeRow = ArchetypeRow (Cons V.empty (unArchetypeRow $ archetypeRow next)), archetypeGetters = Map.empty}

class ToArchetype as a where
  toArchetype' :: a -> Archetype as

instance ToArchetype '[] (EntityRow '[]) where
  toArchetype' _ = Archetype {archetypeRow = ArchetypeRow Nil, archetypeGetters = Map.empty}

instance
  (Typeable a, ToArchetype as (EntityRow as)) =>
  ToArchetype (a : as) (EntityRow (a : as))
  where
  toArchetype' (EntityRow (Cons (Identity e) es)) =
    let Archetype (ArchetypeRow nextRow) nextGs = toArchetype' (EntityRow es)
     in Archetype
          { archetypeRow = ArchetypeRow $ Cons (pure e) nextRow,
            archetypeGetters =
              Map.insert
                (typeOf (Proxy @a))
                (\i (ArchetypeRow (Cons v _)) -> unsafeCoerce <$> v V.!? i)
                (fmap (\f -> \i (ArchetypeRow (Cons _ es')) -> f i (ArchetypeRow es')) nextGs)
          }

toArchetype :: (ToArchetype as (EntityRow as)) => Entity as -> Archetype as
toArchetype = toArchetype' . entityRow

newtype ArchetypesRow as = ArchetypesRow {unArchetypesRow :: Row Archetype as}

data AnyArchetype = forall as. AnyArchetype (Archetype as)

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
