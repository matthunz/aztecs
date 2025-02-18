{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aztecs.Query.Reader
  ( -- * Queries
    QueryReader (..),
    entity,
    fetch,
    fetchMaybe,
    set,
    task,
    all,

    -- * Filters
    QueryFilter (..),
    with,
    without,
    DynamicQueryFilter (..),
  )
where

import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Control.Monad (mapM)
import Data.Aztecs.Component
import Data.Aztecs.Query.Class (ArrowQuery (..))
import Data.Aztecs.Query.Dynamic (DynamicQueryFilter (..))
import Data.Aztecs.Query.Dynamic.Reader (DynamicQueryReader (..))
import Data.Aztecs.Query.Dynamic.Reader.Class (ArrowDynamicQueryReader (..))
import Data.Aztecs.Query.Reader.Class (ArrowQueryReader (..))
import Data.Aztecs.World (World (..))
import qualified Data.Aztecs.World.Archetype as A
import Data.Aztecs.World.Archetypes (Node (nodeArchetype))
import qualified Data.Aztecs.World.Archetypes as AS
import Data.Aztecs.World.Components (Components)
import qualified Data.Aztecs.World.Components as CS
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (all, any, id, lookup, map, mapM, reads, (.))

-- | Query for matching entities.
--
-- === Do notation:
-- > move :: (Monad m) => Query m () Position
-- > move = proc () -> do
-- >   Velocity v <- Q.fetch -< ()
-- >   Position p <- Q.fetch -< ()
-- >   Q.set -< Position $ p + v
--
-- === Arrow combinators:
-- > move :: (Monad m) => Query m () Position
-- > move = Q.fetch &&& Q.fetch >>> arr (\(Position p, Velocity v) -> Position $ p + v) >>> Q.set
--
-- === Applicative combinators:
-- > move :: (Monad m) => Query m () Position
-- > move = (,) <$> Q.fetch <*> Q.fetch >>> arr (\(Position p, Velocity v) -> Position $ p + v) >>> Q.set
newtype QueryReader m i o
  = Query {runQueryReader :: Components -> (Set ComponentID, Components, DynamicQueryReader m i o)}

instance (Functor m) => Functor (QueryReader m i) where
  fmap f (Query q) = Query $ \cs -> let (cIds, cs', qS) = q cs in (cIds, cs', fmap f qS)

instance (Monad m) => Applicative (QueryReader m i) where
  pure a = Query $ \cs -> (mempty, cs, pure a)
  (Query f) <*> (Query g) = Query $ \cs ->
    let (cIdsG, cs', aQS) = g cs
        (cIdsF, cs'', bQS) = f cs'
     in (cIdsG <> cIdsF, cs'', bQS <*> aQS)

instance (Monad m) => Category (QueryReader m) where
  id = Query $ \cs -> (mempty, cs, id)
  (Query f) . (Query g) = Query $ \cs ->
    let (cIdsG, cs', aQS) = g cs
        (cIdsF, cs'', bQS) = f cs'
     in (cIdsG <> cIdsF, cs'', bQS . aQS)

instance (Monad m) => Arrow (QueryReader m) where
  arr f = Query $ \cs -> (mempty, cs, arr f)
  first (Query f) = Query $ \comps -> let (cIds, comps', qS) = f comps in (cIds, comps', first qS)

instance (Monad m) => ArrowQueryReader (QueryReader m) where
  entity = Query $ \cs -> (mempty, cs, entityDyn)
  fetch :: forall a. (Component a) => QueryReader m () a
  fetch = Query $ \cs ->
    let (cId, cs') = CS.insert @a cs
     in (Set.singleton cId, cs', fetchDyn cId)
  fetchMaybe :: forall a. (Component a) => QueryReader m () (Maybe a)
  fetchMaybe = Query $ \cs ->
    let (cId, cs') = CS.insert @a cs
     in (Set.singleton cId, cs', fetchMaybeDyn cId)

-- | Run a monadic task in a `Query`.
task :: (Monad m) => (i -> m o) -> QueryReader m i o
task f = Query $ \cs ->
  ( mempty,
    cs,
    DynamicQueryReader
      { dynQueryReaderAll = \is _ _ -> mapM f is,
        dynQueryReaderLookup = \i _ _ -> (\a -> Just a) <$> f i
      }
  )

-- | Query all matching entities.
--
-- >>> :set -XTypeApplications
-- >>> import Data.Aztecs
-- >>> import qualified Data.Aztecs.World as W
-- >>>
-- >>> newtype X = X Int deriving (Show)
-- >>> instance Component X
-- >>>
-- >>> let (_, w) = W.spawn (bundle $ X 0) W.empty
-- >>> (xs, _) <- all (fetch @_ @X) w
-- >>> xs
-- [X 0]
all :: (Monad m) => QueryReader m () a -> World -> m ([a], World)
all q w = do
  let (rs, cs', dynQ) = runQueryReader q (components w)
  as <-
    mapM
      (\n -> dynQueryReaderAll dynQ (repeat ()) (A.entities $ nodeArchetype n) (nodeArchetype n))
      (Map.elems $ AS.lookup rs (archetypes w))
  return (concat as, w {components = cs'})

-- | Filter for a `Query`.
newtype QueryFilter = QueryFilter {runQueryFilter :: Components -> (DynamicQueryFilter, Components)}

instance Semigroup QueryFilter where
  a <> b =
    QueryFilter
      ( \cs ->
          let (withA', cs') = runQueryFilter a cs
              (withB', cs'') = runQueryFilter b cs'
           in (withA' <> withB', cs'')
      )

instance Monoid QueryFilter where
  mempty = QueryFilter (mempty,)

-- | Filter for entities containing this component.
with :: forall a. (Component a) => QueryFilter
with = QueryFilter $ \cs ->
  let (cId, cs') = CS.insert @a cs in (mempty {filterWith = Set.singleton cId}, cs')

-- | Filter out entities containing this component.
without :: forall a. (Component a) => QueryFilter
without = QueryFilter $ \cs ->
  let (cId, cs') = CS.insert @a cs in (mempty {filterWithout = Set.singleton cId}, cs')
