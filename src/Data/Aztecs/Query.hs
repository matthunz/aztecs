{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Query
  ( ReadWrites (..),
    Query (..),
    entity,
    read,
    write,
    all,
    get,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aztecs.World (Component, Entity, World (..))
import Data.Aztecs.World.Archetypes (Archetype, ArchetypeId, ArchetypeState (..), archetype)
import qualified Data.Aztecs.World.Archetypes as A
import Data.Foldable (foldrM)
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Typeable
import Prelude hiding (all, read)

-- | Component IDs to read and write.
data ReadWrites = ReadWrites (Set TypeRep) (Set TypeRep)

instance Semigroup ReadWrites where
  ReadWrites rs ws <> ReadWrites rs' ws' = ReadWrites (rs <> rs') (ws <> ws')

instance Monoid ReadWrites where
  mempty = ReadWrites mempty mempty

-- | Query to access components.
data Query m a where
  PureQ :: a -> Query m a
  MapQ :: (a -> b) -> Query m a -> Query m b
  AppQ :: Query m (a -> b) -> Query m a -> Query m b
  BindQ :: Query m a -> (a -> Query m b) -> Query m b
  EntityQ :: Query m Entity
  ReadQ :: (Component c) => Archetype -> Query m c
  WriteQ :: (Component c) => (c -> c) -> Archetype -> Query m c
  LiftQ :: m a -> Query m a

instance Functor (Query m) where
  fmap = MapQ

instance Applicative (Query m) where
  pure = PureQ
  (<*>) = AppQ

instance Monad (Query m) where
  (>>=) = BindQ

entity :: Query m Entity
entity = EntityQ

-- | Read a `Component`.
read :: forall m c. (Component c) => Query m c
read = ReadQ (archetype @c)

-- | Alter a `Component`.
write :: forall m c. (Component c) => (c -> c) -> Query m c
write c = WriteQ c (archetype @c)

all :: (MonadIO m) => ArchetypeId -> Query m a -> World -> m [(Entity, a)]
all aId q w@(World _ as) = case A.getArchetype aId as of
  Just s@(ArchetypeState _ m _) ->
    foldrM
      ( \e acc -> do
          a <- get' e s q w
          return $ case a of
            Just a' -> ((e, a') : acc)
            Nothing -> acc
      )
      []
      (Map.keys m)
  Nothing -> return []

get :: (MonadIO m) => ArchetypeId -> Query m a -> Entity -> World -> m (Maybe a)
get a qb e w@(World _ as) = case A.getArchetype a as of
  Just s -> get' e s qb w
  Nothing -> return Nothing

get' :: (MonadIO m) => Entity -> ArchetypeState -> Query m a -> World -> m (Maybe a)
get' _ _ (PureQ a) _ = return $ Just a
get' e es (MapQ f qb) w = do
  a <- get' e es qb w
  return $ fmap f a
get' e es (AppQ fqb aqb) w = do
  f <- get' e es fqb w
  a <- get' e es aqb w
  return $ f <*> a
get' e _ EntityQ _ = return $ Just e
get' e (ArchetypeState _ m _) (ReadQ _) _ = return $ do
  cs <- Map.lookup e m
  (c, _) <- A.getArchetypeComponent cs
  return c
get' e (ArchetypeState _ m _) (WriteQ f _) _ = do
  let res = do
        cs <- Map.lookup e m
        A.getArchetypeComponent cs
  case res of
    Just (c, g) -> do
      let c' = f c
      liftIO $ g c'
      return $ Just c'
    Nothing -> return Nothing
get' e es (BindQ qb f) w = do
  a <- get' e es qb w
  case a of
    Just a' -> get' e es (f a') w
    Nothing -> return Nothing
get' _ _ (LiftQ a) _ = fmap Just a
