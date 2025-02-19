{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.ECS.System.Reader.Class (ArrowReaderSystem (..)) where

import Aztecs.ECS.Component
import Aztecs.ECS.Query.Reader (DynamicQueryFilter (..), QueryFilter (..), QueryReader (..))
import Aztecs.ECS.System.Dynamic.Reader.Class (allDyn', filterDyn')
import Aztecs.ECS.World (World)
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (Node (..))
import Aztecs.ECS.World.Components (Components)
import Control.Arrow (Arrow (..), (>>>))
import qualified Data.Foldable as F
import Data.Set (Set)
import Prelude hiding (all, any, filter, id, lookup, map, mapM, reads, (.))

class (Arrow arr) => ArrowReaderSystem arr where
  -- | Set a `Component` by its type.
  runArrowReaderSystem :: (Components -> (World -> i -> o, Set ComponentID, Components)) -> arr i o

  -- | Query all matching entities.
  all :: (ArrowReaderSystem arr) => QueryReader i a -> arr i [a]
  all q = runArrowReaderSystem $ \cs ->
    let !(rs, cs', dynQ) = runQueryReader q cs
     in (allDyn' rs dynQ, rs, cs')

  -- | Query all matching entities with a `QueryFilter`.
  filter :: (ArrowReaderSystem arr) => QueryReader () a -> QueryFilter -> arr () [a]
  filter q qf = runArrowReaderSystem $ \cs ->
    let !(rs, cs', dynQ) = runQueryReader q cs
        !(dynQf, cs'') = runQueryFilter qf cs'
        qf' n =
          F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynQf)
            && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynQf)
     in (filterDyn' rs dynQ qf', rs, cs'')

  -- | Query a single matching entity.
  -- If there are zero or multiple matching entities, an error will be thrown.
  single :: (ArrowReaderSystem arr) => QueryReader i a -> arr i a
  single q =
    all q
      >>> arr
        ( \as -> case as of
            [a] -> a
            _ -> error "TODO"
        )
