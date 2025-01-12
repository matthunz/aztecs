{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aztecs.World where

import Data.Aztecs (EntityID (..))
import Data.Aztecs.Archetypes
import Data.Aztecs.Entity (Entity)
import Data.Kind (Type)
import Data.Vector (Vector)
import qualified Data.Vector as V

type family CombineT (a :: Type) (as :: [Type]) :: [[Type]] where
  CombineT a '[] = '[ '[a]]
  CombineT a (b ': bs) = '[a] ': '[b] ': '[a, b] ': CombineT a bs

type family CombineT' (as :: [Type]) :: [[Type]] where
  CombineT' '[] = '[]
  CombineT' (a ': as) = CombineT a as

data World cs = World (Archetypes (CombineT' cs)) EntityID

instance (Show (Archetypes (CombineT' cs))) => Show (World cs) where
  show (World as e) = "World " ++ show as ++ show e

class Empty (as :: [[Type]]) where
  empty :: Archetypes as

instance Empty '[] where
  empty = ASNil

instance (EmptyArchetype a, Empty as) => Empty (a ': as) where
  empty = ASCons emptyArchetype empty

class Spawn (a :: [Type]) (as :: [Type]) where
  spawn :: Entity a -> World as -> World as

instance (SpawnArchetypes a (CombineT' as)) => Spawn a as where
  spawn x (World as e) = World (spawnArchetypes x as) (EntityID $ unEntityId e + 1)

world :: (Empty (CombineT' cs)) => World cs
world = World empty (EntityID 0)
