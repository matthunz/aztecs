{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aztecs.Archetype where

import Data.Kind (Type)
import Data.Vector (Vector)

data Archetype (as :: [Type]) where
  ENil :: Archetype '[]
  ECons :: Vector a -> Archetype as -> Archetype (a ': as)

instance Show (Archetype '[]) where
  show ENil = "[]"

instance (Show a, ShowArchetype (Archetype as)) => Show (Archetype (a ': as)) where
  show (ECons x xs) = "[ " ++ show x ++ showArchetype xs

class ShowArchetype a where
  showArchetype :: a -> String

instance ShowArchetype (Archetype '[]) where
  showArchetype ENil = "]"

instance (Show a, ShowArchetype (Archetype as)) => ShowArchetype (Archetype (a ': as)) where
  showArchetype (ECons x xs) = ", " ++ show x ++ showArchetype xs
