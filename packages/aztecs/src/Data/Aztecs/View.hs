{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.View
  ( View (..),
    view,
    filterView,
    unview,
    allDyn,
    readAllDyn,
  )
where

import Data.Aztecs.Query.Dynamic (DynamicQuery (..))
import Data.Aztecs.Query.Dynamic.Reader (DynamicQueryReader (..))
import Data.Aztecs.World (World)
import qualified Data.Aztecs.World as W
import qualified Data.Aztecs.World.Archetype as A
import Data.Aztecs.World.Archetypes (ArchetypeID, Archetypes, Node (..))
import qualified Data.Aztecs.World.Archetypes as AS
import Data.Aztecs.World.Components (ComponentID)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)

#if !MIN_VERSION_base(4,20,0)
import Data.Foldable (foldl')
#endif

-- | View into a `World`, containing a subset of archetypes.
newtype View = View {viewArchetypes :: Map ArchetypeID Node}
  deriving (Show, Semigroup, Monoid)

-- | View into all archetypes containing the provided component IDs.
view :: Set ComponentID -> Archetypes -> View
view cIds as = View $ AS.lookup cIds as

-- | View into all archetypes containing the provided component IDs and matching the provided predicate.
filterView ::
  Set ComponentID ->
  (Node -> Bool) ->
  Archetypes ->
  View
filterView cIds f as = View $ Map.filter f (AS.lookup cIds as)

-- | "Un-view" a `View` back into a `World`.
unview :: View -> World -> World
unview v w =
  w
    { W.archetypes =
        foldl'
          (\as (aId, n) -> as {AS.nodes = Map.insert aId n (AS.nodes as)})
          (W.archetypes w)
          (Map.toList $ viewArchetypes v)
    }

-- | Query all matching entities in a `View`.
allDyn :: i -> DynamicQuery i a -> View -> ([a], View)
allDyn i q v =
  let (as, arches) =
        foldl'
          ( \(acc, archAcc) (aId, n) ->
              let (as', arch') = dynQueryAll q (repeat i) (A.entities (nodeArchetype n)) (nodeArchetype n)
               in (as' ++ acc, Map.insert aId (n {nodeArchetype = arch'}) archAcc)
          )
          ([], Map.empty)
          (Map.toList $ viewArchetypes v)
   in (as, View arches)

-- | Query all matching entities in a `View`.
readAllDyn :: i -> DynamicQueryReader i a -> View -> [a]
readAllDyn i q v =
  foldl'
    ( \acc n ->
        dynQueryReaderAll q (repeat i) (A.entities (nodeArchetype n)) (nodeArchetype n) ++ acc
    )
    []
    (viewArchetypes v)
