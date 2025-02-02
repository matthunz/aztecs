{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow ((>>>))
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import Data.Aztecs.SDL (Window (..), load, sdlPlugin)
import qualified Data.Aztecs.System as S
import Data.Aztecs.Transform (Transform (transformPosition), transform)
import SDL hiding (Window, windowTitle)

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity

data Setup

instance System IO Setup where
  task =
    S.mapWith
      ( \assetServer -> do
          (assetId, assetServer') <- load "example.png" assetServer
          return (assetId, assetServer')
      )
      >>> S.queueWith
        ( \res -> case res of
            [(assetId, _)] -> do
              A.spawn_ (Window {windowTitle = "Aztecs"})
              A.spawn_ $ assetId :& transform {transformPosition = V2 100 100}
            _ -> error "TODO"
        )

app :: Scheduler IO
app = sdlPlugin <> schedule @_ @Startup @Setup []

main :: IO ()
main = run app
