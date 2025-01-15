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
  )
where

import Control.Monad.Identity (Identity (Identity))
import Data.Data (TypeRep, Typeable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable (Proxy (..), typeOf)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Base (Any)
import Unsafe.Coerce (unsafeCoerce)

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

instance (Has (Identity a) (Row Identity as)) => Has a (EntityRow as) where
  get e = let (Identity a) = get (unEntityRow e) in a

data Entity as = Entity
  { entityRow :: EntityRow as,
    getters :: Map TypeRep (EntityRow as -> Any)
  }

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

newtype ArchetypeRow as = ArchetypeRow {unArchetypeRow :: Row Vector as}

data Archetype as = Archetype
  { archetypeRow :: ArchetypeRow as,
    archetypeGetters :: Map TypeRep (Int -> ArchetypeRow as -> Maybe Any)
  }

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

data World = forall as. World (Archetypes as) (Map EntityID (Record as)) (EntityID)

world :: World
world = World (Archetypes (ArchetypesRow Nil) Map.empty) Map.empty (EntityID 0)

addArchetype :: forall a as. (Typeable a) => Archetype a -> Archetypes as -> Archetypes (a : as)
addArchetype a (Archetypes (ArchetypesRow row) gs) =
  Archetypes
    (ArchetypesRow (Cons a row))
    ( Map.insert
        (typeOf (Proxy @a))
        (\(ArchetypesRow (Cons a' _)) -> AnyArchetype a')
        (fmap (\f -> \(ArchetypesRow (Cons _ row')) -> f (ArchetypesRow row')) gs)
    )

spawn :: (Typeable a, ToArchetype a (EntityRow a)) => Entity a -> World -> (EntityID, World)
spawn e (World as rs i) =
  ( i,
    World
      (addArchetype (toArchetype e) as)
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

