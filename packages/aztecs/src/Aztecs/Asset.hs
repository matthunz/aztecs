{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.Asset where

import Aztecs.ECS
import qualified Aztecs.ECS.Access as A
import Aztecs.ECS.Query (ArrowQuery)
import qualified Aztecs.ECS.Query as Q
import Aztecs.ECS.System (ArrowSystem)
import qualified Aztecs.ECS.System as S
import Control.Arrow (returnA)
import Control.Concurrent (forkIO)
import Control.Monad.Identity (Identity)
import Control.Monad.State.Strict (MonadState (..), StateT, runState)
import Data.Data (Typeable)
import Data.Foldable (foldrM)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

newtype AssetId = AssetId {unAssetId :: Int}
  deriving (Eq, Ord, Show)

data AssetServer a = AssetServer
  { assetServerAssets :: !(Map AssetId a),
    loadingAssets :: !(Map AssetId (Either (IO (IORef (Maybe a))) (IORef (Maybe a)))),
    nextAssetId :: !AssetId
  }

instance (Typeable a) => Component (AssetServer a)

empty :: AssetServer a
empty =
  AssetServer
    { assetServerAssets = Map.empty,
      loadingAssets = Map.empty,
      nextAssetId = AssetId 0
    }

class (Typeable a) => Asset a where
  type AssetConfig a

  loadAsset :: FilePath -> AssetConfig a -> IO a

newtype Handle a = Handle {handleId :: AssetId}
  deriving (Eq, Ord, Show)

class MonadAssetServer a m | m -> a where
  asset :: FilePath -> AssetConfig a -> m (Handle a)

type AssetLoader a o = AssetLoaderT a Identity o

newtype AssetLoaderT a m o = AssetLoaderT {unAssetLoader :: StateT (AssetServer a) m o}
  deriving (Functor, Applicative, Monad)

instance (Monad m, Asset a) => MonadAssetServer a (AssetLoaderT a m) where
  asset path cfg = AssetLoaderT $ do
    server <- get
    let assetId = nextAssetId server
        go = do
          v <- newIORef Nothing
          _ <- forkIO $ do
            a <- loadAsset path cfg
            writeIORef v (Just a)
          return $ v
    put $
      server
        { loadingAssets = Map.insert assetId (Left go) (loadingAssets server),
          nextAssetId = AssetId (unAssetId assetId + 1)
        }
    return $ Handle assetId

loadQuery :: (Asset a, ArrowQuery arr) => AssetLoader a o -> arr () o
loadQuery a = proc () -> do
  assetServer <- Q.fetch -< ()
  let (o, assetServer') = runState (unAssetLoader a) assetServer
  Q.set -< assetServer'
  returnA -< o

load :: (ArrowSystem arr, Asset a) => AssetLoader a o -> arr () o
load a = S.mapSingle $ loadQuery a

lookupAsset :: Handle a -> AssetServer a -> Maybe a
lookupAsset h server = Map.lookup (handleId h) (assetServerAssets server)

loadAssets :: forall a. (Typeable a) => Schedule IO () ()
loadAssets = proc () -> do
  server <- reader $ S.single (Q.fetch @_ @(AssetServer a)) -< ()
  server' <-
    task
      ( \server ->
          foldrM
            ( \(aId, v) acc -> do
                case v of
                  Right r -> do
                    maybeSurface <- readIORef r
                    case maybeSurface of
                      Just surface ->
                        return
                          acc
                            { assetServerAssets = Map.insert aId surface (assetServerAssets acc),
                              loadingAssets = Map.delete aId (loadingAssets acc)
                            }
                      Nothing -> return acc
                  Left f -> do
                    v' <- f
                    return $ acc {loadingAssets = Map.insert aId (Right v') (loadingAssets server)}
            )
            server
            (Map.toList $ loadingAssets server)
      )
      -<
        server
  system $ S.mapSingle Q.set -< server'
  returnA -< ()

setup :: forall a. (Typeable a) => System () ()
setup = S.queue . const . A.spawn_ . bundle $ empty @a
