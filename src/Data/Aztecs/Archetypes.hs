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

module Data.Aztecs.Archetypes where

import Data.Aztecs.Archetype (Archetype (..))
import Data.Aztecs.Entity (Entity (..))
import Data.Kind (Type)
import qualified Data.Vector as V

data Archetypes (ts :: [[Type]]) where
  ASNil :: Archetypes '[]
  ASCons :: Archetype as -> Archetypes ts -> Archetypes (as ': ts)

instance Show (Archetypes '[]) where
  show ASNil = "[]"

instance (Show (Archetype as), ShowArchetypes (Archetypes es)) => Show (Archetypes (as ': es)) where
  show (ASCons x xs) = "[" ++ show x ++ showArchetypes xs

class ShowArchetypes a where
  showArchetypes :: a -> String

instance ShowArchetypes (Archetypes '[]) where
  showArchetypes ASNil = "]"

instance (Show (Archetype cs), ShowArchetypes (Archetypes as)) => ShowArchetypes (Archetypes (cs ': as)) where
  showArchetypes (ASCons x xs) = ", " ++ show x ++ showArchetypes xs

class EmptyArchetype (as :: [Type]) where
  emptyArchetype :: Archetype as

instance EmptyArchetype '[] where
  emptyArchetype = ANil

instance (EmptyArchetype as) => EmptyArchetype (a ': as) where
  emptyArchetype = ACons V.empty emptyArchetype

class SpawnArchetype (a :: [Type]) (as :: [Type]) where
  spawnArchetype :: Entity a -> Archetype as -> Archetype as

instance SpawnArchetype '[] as where
  spawnArchetype _ as = as

instance (SpawnArchetype as as) => SpawnArchetype (a ': as) (a ': as) where
  spawnArchetype (ECons e es) (ACons v as) = ACons (V.cons e v) (spawnArchetype es as)

class EmptyArchetypes (as :: [[Type]]) where
  emptyArchetypes :: Archetypes as

class SpawnArchetypes (a :: [Type]) (as :: [[Type]]) where
  spawnArchetypes :: Entity a -> Archetypes as -> Archetypes as

instance (SpawnArchetype a a) => SpawnArchetypes a (a ': as) where
  spawnArchetypes e (ASCons x xs) = ASCons (spawnArchetype e x) xs
