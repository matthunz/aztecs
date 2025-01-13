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

import Data.Aztecs (Has (..))
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

instance {-# OVERLAPPING #-} Has (Archetype as) (Archetypes (as ': as')) where
  component (ASCons x _) = x
  setComponent x (ASCons _ xs) = ASCons x xs

instance {-# OVERLAPPING #-} (Has (Archetype cs) (Archetypes as)) => Has (Archetype cs) (Archetypes (bs ': as)) where
  component (ASCons _ xs) = component xs
  setComponent x (ASCons y xs) = ASCons y (setComponent x xs)

class EmptyArchetype (as :: [Type]) where
  emptyArchetype :: Archetype as

instance EmptyArchetype '[] where
  emptyArchetype = ANil

instance (EmptyArchetype as) => EmptyArchetype (a ': as) where
  emptyArchetype = ACons V.empty emptyArchetype

class SpawnArchetype (as :: [Type]) where
  spawnArchetype :: Entity as -> Archetype as -> Archetype as

instance SpawnArchetype as where
  spawnArchetype ENil ANil = ANil
  spawnArchetype (ECons e es) (ACons v as) = ACons (V.cons e v) (spawnArchetype es as)

class SpawnArchetypes (a :: [Type]) (as :: [[Type]]) where
  spawnArchetypes :: Entity a -> Archetypes as -> Archetypes as

instance  {-# OVERLAPPING #-} SpawnArchetypes a '[] where
  spawnArchetypes _ ASNil = ASNil

instance {-# OVERLAPPING #-} (SpawnArchetype a) => SpawnArchetypes a '[a] where
  spawnArchetypes e (ASCons x xs) = ASCons (spawnArchetype e x) xs

instance {-# OVERLAPPING #-}  (SpawnArchetype a) => SpawnArchetypes a (a ': as) where
  spawnArchetypes e (ASCons x xs) = ASCons (spawnArchetype e x) xs

instance  {-# OVERLAPPING #-} (SpawnArchetypes a as) => SpawnArchetypes a (b ': as) where
  spawnArchetypes e (ASCons x xs) = ASCons x (spawnArchetypes e xs)
