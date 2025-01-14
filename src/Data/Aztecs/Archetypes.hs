{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
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
import Data.Aztecs.Archetype (Archetype, Subset)
import qualified Data.Aztecs.Archetype as A
import Data.Aztecs.Entity
import Data.Data (Proxy)
import Data.Kind (Type)
import Prelude hiding (lookup, map)

data Archetypes (as :: [[Type]]) where
  ANil :: Archetypes '[]
  ACons :: Archetype a -> Archetypes as -> Archetypes (a ': as)

instance Show (Archetypes '[]) where
  show ANil = "[]"

instance (Show (Archetype a), ShowArchetypes (Archetypes as)) => Show (Archetypes (a ': as)) where
  show (ACons x xs) = "[ " ++ show x ++ showArchetypes xs

class ShowArchetypes a where
  showArchetypes :: a -> String

instance ShowArchetypes (Archetypes '[]) where
  showArchetypes ANil = "]"

instance (Show (Archetype a), ShowArchetypes (Archetypes as)) => ShowArchetypes (Archetypes (a ': as)) where
  showArchetypes (ACons x xs) = ", " ++ show x ++ showArchetypes xs

instance {-# OVERLAPPING #-} Has (Archetype as) (Archetypes (as ': as')) where
  component (ACons x _) = x
  setComponent x (ACons _ xs) = ACons x xs

instance {-# OVERLAPPING #-} (Has (Archetype cs) (Archetypes as)) => Has (Archetype cs) (Archetypes (bs ': as)) where
  component (ACons _ xs) = component xs
  setComponent x (ACons y xs) = ACons y (setComponent x xs)

class Match (as :: [[Type]]) (es :: [Type]) where
  match :: Archetypes as -> [Entity es]

class Match' (flag :: Bool) (as :: [[Type]]) (es :: [Type]) where
  match' :: Archetypes as -> [Entity es]

instance (A.Match a es, Match as es) => Match' 'True (a ': as) es where
  match' (ACons a rest) =
    let currentMatches = A.match a
        otherMatches = match rest
     in currentMatches ++ otherMatches

instance (Match as es) => Match' 'False (a ': as) es where
  match' (ACons _ rest) = match rest

instance forall es a as flag. (Subset es a ~ flag, Match' flag (a ': as) es) => Match (a ': as) es where
  match as = match' @flag as

instance Match '[] es where
  match _ = []

class Map (es :: [Type]) (as :: [[Type]]) where
  map :: (Entity es -> Entity es) -> Archetypes as -> Archetypes as

class Map' (flag :: Bool) (es :: [Type]) (as :: [[Type]]) where
  map' :: (Entity es -> Entity es) -> Archetypes as -> Archetypes as

instance Map' flag as '[] where
  map' _ as = as

instance Map '[] as where
  map _ as = as

instance Map es '[] where
  map _ as = as

instance (A.Match a es, Map es as) => Map' 'True es (a ': as) where
  map' f (ACons a rest) =
    let currentMatches = A.map @_ @es a f
     in ACons (currentMatches) (map f rest)

instance (Map es as) => Map' 'False es (a ': as) where
  map' f (ACons a rest) = ACons a (map f rest)

instance
  forall es a as flag.
  (Subset es a ~ flag, Map' flag es (a ': as), Map' flag es as) =>
  Map es (a ': as)
  where
  map f as = map' @flag f as

class MatchMaybe' (as :: [[Type]]) (es :: [Type]) where
  matchMaybe' :: Archetypes as -> [(Entity es)]

instance MatchMaybe' '[] es where
  matchMaybe' _  = []

instance (A.MatchMaybe a es, MatchMaybe' as es) => MatchMaybe' (a ': as) es where
  matchMaybe' (ACons a rest) = A.matchMaybe @a @es a ++ matchMaybe' rest

class MatchMaybe (as :: [[Type]]) (es :: [Type]) where
  matchMaybe ::
    forall a.
    (A.MatchMaybe' (Subset es a) a es, Has (Archetype a) (Archetypes as)) =>
    Proxy a ->
    Archetypes as ->
    [Entity es]

instance MatchMaybe as es where
  matchMaybe ::
    forall a.
    (A.MatchMaybe' (Subset es a) a es, Has (Archetype a) (Archetypes as)) =>
    Proxy a ->
    Archetypes as ->
    [Entity es]
  matchMaybe _ as = A.matchMaybe @a @es (component as)
