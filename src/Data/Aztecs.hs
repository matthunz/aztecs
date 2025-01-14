{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aztecs (EntityID (..)) where

import Control.Monad.Identity (Identity)
import Data.Kind (Type)
import Data.Vector (Vector)

newtype EntityID = EntityID {unEntityID :: Int}
  deriving (Eq, Ord, Show)

data Row f (as :: [Type]) where
  Nil :: Row f '[]
  Cons :: f a -> Row f as -> Row f (a ': as)

newtype Entity as = Entity {unEntity :: Row Identity as}

newtype Archetype as = Archetype {unArchetype :: Row Vector as}
