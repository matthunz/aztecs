{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aztecs.World where

import Data.Aztecs (EntityID (..), Has (..))
import qualified Data.Aztecs.Archetype as A
import Data.Aztecs.Archetypes
import Data.Aztecs.Entity (Entity)
import Data.Dynamic (Typeable, fromDynamic, toDyn)
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map

type family CombineT (a :: Type) (as :: [Type]) :: [[Type]] where
  CombineT a '[] = '[ '[a]]
  CombineT a (b ': bs) = '[a] ': '[b] ': '[a, b] ': CombineT a bs

type family CombineT' (as :: [Type]) :: [[Type]] where
  CombineT' '[] = '[]
  CombineT' (a ': as) = CombineT a as

data Record cs
  = forall es.
    (Typeable es) =>
    Record
      ( Int ->
        (World cs) ->
        (Maybe (Entity es))
      )

data World cs = World
  { archetypes :: Archetypes (CombineT' cs),
    records :: Map EntityID (Record cs),
    nextEntityId :: EntityID
  }

instance (Show (Archetypes (CombineT' cs))) => Show (World cs) where
  show (World as _ e) = "World " ++ show as ++ show e

class Empty (as :: [[Type]]) where
  empty :: Archetypes as

instance Empty '[] where
  empty = ASNil

instance (EmptyArchetype a, Empty as) => Empty (a ': as) where
  empty = ASCons emptyArchetype empty

spawn ::
  forall a as cs.
  ( Typeable a,
    SpawnArchetypes a (CombineT' cs),
    A.Lookup as a,
    Has (A.Archetype as) (Archetypes (CombineT' cs))
  ) =>
  Entity a ->
  World cs ->
  (EntityID, World cs)
spawn x (World as rs e) =
  let r = Record @cs $ \i (World as' _ _) ->
        let arch = component @(A.Archetype as) as'
         in A.lookup @as @a i arch
   in ( e,
        World
          (spawnArchetypes x as)
          (Map.insert e r rs)
          (EntityID $ unEntityId e + 1)
      )

world :: (Empty (CombineT' cs)) => World cs
world = World empty Map.empty (EntityID 0)

lookup :: (Typeable es) => EntityID -> World cs -> Maybe (Entity es)
lookup i w = do
  (Record f) <- Map.lookup i (records w)
  e <- f 0 w
  let dyn = toDyn e
  fromDynamic dyn

{-

lookup ::
  forall as es cs.
  (A.Lookup as es, Has (A.Archetype as) (Archetypes (CombineT' cs))) =>
  Int ->
  World cs ->
  Maybe (Entity es)
lookup i (World as _) =
  A.lookup @as @es i (component as)

-}