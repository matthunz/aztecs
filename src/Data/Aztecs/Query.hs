{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Query
  ( Query (..),
    read,
    write,
    writeWith,
    all,
    all',
    lookup,
    lookup',
    QueryBuilder (..),
    build,
    QueryT (..),
    lookupT,
    readT,
  )
where

import Control.Monad.Reader (MonadReader (ask), MonadTrans (lift), Reader, ReaderT (runReaderT), runReader)
import Control.Monad.State (MonadState (..), State, StateT (runStateT))
import Control.Monad.Writer (MonadWriter (..), Writer, WriterT (runWriterT))
import Data.Aztecs
import Data.Aztecs.Archetypes
import qualified Data.Aztecs.Archetypes as AS
import Data.Aztecs.Command (Command (..))
import qualified Data.Aztecs.Components as CS
import Data.Aztecs.Table (Column)
import qualified Data.Aztecs.Table as Table
import Data.Aztecs.World (World (..))
import qualified Data.Aztecs.World as W
import Data.Data (Typeable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Prelude hiding (all, lookup, read)

newtype QueryT m a = QueryT (StateT Column (ReaderT (ArchetypeID, World) m) (Maybe a))
  deriving (Functor)

instance (Monad m) => Applicative (QueryT m) where
  pure a = QueryT $ pure (Just a)
  (QueryT f) <*> (QueryT a) = QueryT $ do
    a' <- a
    case a' of
      Just a'' -> do
        f' <- f
        case f' of
          Just f'' -> do
            let x = f'' a''
            return $ Just x
          Nothing -> return Nothing
      Nothing -> return Nothing

instance (Monad m) => Monad (QueryT m) where
  (QueryT a) >>= f = QueryT $ do
    a' <- a
    case a' of
      Just a'' ->
        let (QueryT q) = f a''
         in q
      Nothing -> return Nothing

runQueryT :: QueryT m a -> Column -> ArchetypeID -> World -> m (Maybe a, Column)
runQueryT (QueryT q) col archId w = do
  runReaderT (runStateT q col) (archId, w)

build :: (Monad m) => QueryBuilder m a -> Command m (ComponentIDSet, a)
build qb = Command $ do
  w <- get
  (a, idSet, w') <- lift $ buildQuery qb w
  put w'
  return (idSet, a)

lookupT :: (Monad m) => Entity -> QueryBuilder m (QueryT m a) -> Command m (Maybe a)
lookupT e qb = do
  (idSet, q) <- build qb
  lookupQueryT e idSet q

lookupQueryT :: (Monad m) => Entity -> ComponentIDSet -> QueryT m a -> Command m (Maybe a)
lookupQueryT e idSet q = Command $ do
  w <- get
  res <- lift $ lookupT' e idSet q w
  case res of
    Just (a, w') -> do
      put w'
      return $ Just a
    Nothing -> return Nothing

lookupT' :: (Monad m) => Entity -> ComponentIDSet -> QueryT m a -> World -> m (Maybe (a, World))
lookupT' e idSet q w = do
  let res = do
        archId <- Map.lookup idSet (archetypeIds (W.archetypes w))
        let arch = (AS.archetypes (W.archetypes w)) Map.! archId
        record <- Map.lookup e (entities (W.archetypes w))
        col <- Table.lookupColumn (recordTableId record) (archetypeTable arch)
        return (archId, arch, recordTableId record, col)
  case res of
    Just (archId, arch, tableId, col) -> do
      (a, col') <- runQueryT q col archId w
      let archs = W.archetypes w
      case a of
        Just a' ->
          return $
            Just
              ( a',
                w
                  { W.archetypes =
                      archs
                        { AS.archetypes =
                            Map.insert
                              archId
                              ( arch
                                  { archetypeTable =
                                      Table.insertCol tableId col' (archetypeTable arch)
                                  }
                              )
                              (AS.archetypes $ archs)
                        }
                  }
              )
        Nothing -> return Nothing
    Nothing -> return Nothing

newtype QueryBuilder m a = QueryBuilder (StateT World (WriterT ComponentIDSet m) a)
  deriving (Functor, Applicative, Monad)

buildQuery :: (Monad m) => QueryBuilder m a -> World -> m (a, ComponentIDSet, World)
buildQuery (QueryBuilder qb) w = do
  ((a, w'), idSet) <- runWriterT $ runStateT qb w
  return (a, idSet, w')

readT :: forall m c. (Monad m, Typeable c) => QueryBuilder m (QueryT m c)
readT = QueryBuilder $ do
  w <- get
  let (cId, cs) = CS.insert @c (components w)
  put (w {components = cs})

  tell . ComponentIDSet $ Set.singleton cId

  return . QueryT $ do
    (archId, wAcc) <- ask
    col <- get
    let res = do
          cState <- Map.lookup cId (componentStates (W.archetypes wAcc))
          colId <- Map.lookup archId (componentColumnIds cState)
          c <- Table.lookupColumnId colId col
          return (c, col)
    case res of
      Just (c', col') -> do
        put col'
        return $ Just c'
      Nothing -> return Nothing

newtype Query a
  = Query (World -> (ComponentIDSet, World, ArchetypeID -> Column -> World -> Maybe (a, Column)))
  deriving (Functor)

instance Applicative Query where
  pure a = Query $ \w -> (ComponentIDSet Set.empty, w, \_ _ _ -> Just (a, mempty))
  Query f <*> Query a = Query $ \w ->
    let (ComponentIDSet idSetF, w', f'') = f w
        (ComponentIDSet idSetA, w'', a'') = a w'
     in ( ComponentIDSet $ Set.union idSetF idSetA,
          w'',
          \archId col wAcc -> do
            (f', col') <- f'' archId col wAcc
            (a', col'') <- a'' archId col' wAcc
            return (f' a', col'')
        )

read :: forall c. (Typeable c) => Query c
read = Query $ \w ->
  let (cId, cs) = CS.insert @c (components w)
   in ( ComponentIDSet (Set.singleton cId),
        w {components = cs},
        \archId col wAcc -> do
          cState <- Map.lookup cId (componentStates (W.archetypes wAcc))
          colId <- Map.lookup archId (componentColumnIds cState)
          c <- Table.lookupColumnId colId col
          return (c, col)
      )

write :: forall c. (Typeable c) => (c -> c) -> Query c
write f = Query $ \w ->
  let (cId, cs) = CS.insert @c (components w)
   in ( ComponentIDSet (Set.singleton cId),
        w {components = cs},
        \archId col wAcc -> do
          cState <- Map.lookup cId (componentStates (W.archetypes wAcc))
          colId <- Map.lookup archId (componentColumnIds cState)
          c <- Table.lookupColumnId colId col
          let c' = f c
          return (c', Table.colInsert colId c' col)
      )

writeWith :: forall a b c. (Typeable c) => Query a -> (a -> c -> (c, b)) -> Query b
writeWith (Query q) f = Query $ \w ->
  let (cId, cs) = CS.insert @c (components w)
      (cIds, w', g) = q w
   in ( ComponentIDSet (Set.insert cId (unComponentIdSet cIds)),
        w' {components = cs},
        \archId col wAcc -> do
          (a, col') <- g archId col wAcc
          cState <- Map.lookup cId (componentStates (W.archetypes wAcc))
          colId <- Map.lookup archId (componentColumnIds cState)
          c <- Table.lookupColumnId colId col'
          let (c', b) = f a c
          return (b, Table.colInsert colId c' col')
      )

all :: (Monad m) => Query a -> Command m [a]
all q = Command $ do
  w <- get
  let (as, w') = all' q w
  put w'
  return as

all' :: Query a -> World -> ([a], World)
all' (Query f) w =
  case f w of
    (idSet, w', f') -> case Map.lookup idSet (archetypeIds (W.archetypes w')) of
      Just archId ->
        let go i (cAcc, wAcc) =
              let arch = AS.archetypes (W.archetypes w') Map.! i
                  ((cs', cols), wAcc') = (unzip $ fromMaybe [] $ mapM (\col -> f' archId col w') (Table.toList (archetypeTable arch)), wAcc)
                  (cs'', wAcc'') = foldr go ([], wAcc') (Map.elems $ archetypeAdd arch)
                  archs = (W.archetypes wAcc'')
                  wAcc''' =
                    wAcc''
                      { W.archetypes =
                          archs
                            { AS.archetypes =
                                Map.insert
                                  archId
                                  (arch {archetypeTable = Table.fromList cols})
                                  (AS.archetypes $ archs)
                            }
                      }
               in (cAcc ++ cs' ++ cs'', wAcc''')
            (cs, w'') = go archId ([], w')
         in (cs, w'')
      Nothing -> ([], w')

lookup' :: Entity -> Query a -> World -> Maybe (a, World)
lookup' e (Query f) w =
  case f w of
    (idSet, w', f') -> do
      archId <- Map.lookup idSet (archetypeIds (W.archetypes w'))
      let arch = (AS.archetypes (W.archetypes w')) Map.! archId
      record <- Map.lookup e (entities (W.archetypes w'))
      col <- Table.lookupColumn (recordTableId record) (archetypeTable arch)
      (a, col') <- f' archId col w'
      let archs = W.archetypes w'
      return
        ( a,
          w'
            { W.archetypes =
                archs
                  { AS.archetypes =
                      Map.insert
                        archId
                        ( arch
                            { archetypeTable =
                                Table.insertCol (recordTableId record) col' (archetypeTable arch)
                            }
                        )
                        (AS.archetypes $ archs)
                  }
            }
        )

lookup :: (Monad m) => Entity -> Query a -> Command m (Maybe a)
lookup e q = Command $ do
  w <- get
  case lookup' e q w of
    Just (a, w') -> do
      put w'
      return $ Just a
    Nothing -> return Nothing
