{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aztecs.Entity where

import Data.Aztecs (Component, EntityID)
import qualified Data.Aztecs.Components as CS
import Data.Aztecs.Table (Table)
import qualified Data.Aztecs.Table as Table
import Data.Aztecs.World (ArchetypeID, ComponentIDSet (..), World)
import qualified Data.Aztecs.World as W
import Data.Kind (Type)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Prelude hiding (all)

-- | Entity of components.
data Entity (ts :: [Type]) where
  ENil :: Entity '[]
  ECons :: t -> Entity ts -> Entity (t ': ts)

instance Show (Entity '[]) where
  show ENil = "[]"

instance (Show t, ShowEntity (Entity ts)) => Show (Entity (t ': ts)) where
  show (ECons x xs) = "[ " ++ show x ++ showEntity xs

class ShowEntity a where
  showEntity :: a -> String

instance ShowEntity (Entity '[]) where
  showEntity ENil = "]"

instance (Show t, ShowEntity (Entity ts)) => ShowEntity (Entity (t ': ts)) where
  showEntity (ECons x xs) = ", " ++ show x ++ showEntity xs

-- | Component accessor.
class Has a l where
  -- | Get a component.
  component :: l -> a

instance {-# OVERLAPPING #-} Has a (Entity (a ': ts)) where
  component (ECons x _) = x

instance {-# OVERLAPPING #-} (Has a (Entity ts)) => Has a (Entity (b ': ts)) where
  component (ECons _ xs) = component xs

-- | Create an entity from a component.
entity :: t -> Entity '[t]
entity t = ECons t ENil

-- | Append a component to an entity.
(<&>) :: Entity ts -> t -> Entity (t ': ts)
(<&>) = flip ECons

data (:&) a b = (:&) a b

type family EntityT a where
  EntityT (a :& b) = a ': EntityT b
  EntityT (Entity ts) = ts
  EntityT a = '[a]

-- | Convert from an @Entity@.
class FromEntity a where
  fromEntity :: Entity (EntityT a) -> a

instance {-# OVERLAPS #-} (EntityT a ~ '[a]) => FromEntity a where
  fromEntity (ECons a ENil) = a

instance FromEntity (Entity ts) where
  fromEntity = id

instance (FromEntity a, FromEntity b, EntityT (a :& b) ~ (a ': EntityT b)) => FromEntity (a :& b) where
  fromEntity (ECons a rest) = a :& fromEntity rest

-- | Convert to an @Entity@.
class ToEntity a where
  toEntity :: a -> Entity (EntityT a)

instance {-# OVERLAPS #-} (EntityT a ~ '[a]) => ToEntity a where
  toEntity a = ECons a ENil

instance ToEntity (Entity ts) where
  toEntity = id

instance (ToEntity a, ToEntity b, EntityT (a :& b) ~ (a ': EntityT b)) => ToEntity (a :& b) where
  toEntity (a :& b) = ECons a (toEntity b)

-- | Insertable entity.
class Insertable a where
  spawn :: a -> World -> (EntityID, World)
  insert :: EntityID -> a -> World -> World

instance Insertable (Entity '[]) where
  spawn ENil w = (W.nextEntityId w, w)
  insert _ ENil w = w

instance (Component a, Insertable (Entity as)) => Insertable (Entity (a ': as)) where
  spawn (ECons x xs) w = let (e, w') = W.spawn x w in (e, insert e xs w')
  insert eId (ECons x xs) w = insert eId xs $ W.insert eId x w

class Queryable a where
  all :: World -> (ComponentIDSet, World, Table -> ArchetypeID -> World -> [a])

instance {-# OVERLAPS #-} (Component a) => Queryable (Entity '[a]) where
  all w =
    let (cId, cs) = CS.insert @a (W.components w)
        w' = w {W.components = cs}
        f table archId wAcc =
          let cState = W.componentStates wAcc Map.! cId
              colId = W.componentColumnIds cState Map.! archId
              as = Table.toList @a colId table
           in map (\a -> ECons a ENil) as
     in (ComponentIDSet (Set.singleton cId), w', f)

instance {-# OVERLAPS #-} (Component a, Queryable (Entity as)) => Queryable (Entity (a ': as)) where
  all w =
    let (cId, cs) = CS.insert @a (W.components w)
        w' = w {W.components = cs}
        (idSet, w'', g) = all @(Entity as) w'
        f table archId wAcc =
          let cState = W.componentStates wAcc Map.! cId
              colId = W.componentColumnIds cState Map.! archId
              as = Table.toList @a colId table
              es = g table archId wAcc
           in zipWith (\a e -> ECons a e) as es
     in (ComponentIDSet (Set.singleton cId) <> idSet, w'', f)