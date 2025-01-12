{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class
import Data.Aztecs (Component)
import Data.Aztecs.Access (Access)
import qualified Data.Aztecs.Access as A
import Data.Aztecs.Entity
import qualified Data.Aztecs.World as W
import Text.Pretty.Simple

newtype X = X Int deriving (Show)

instance Component X

newtype Y = Y Int deriving (Show)

instance Component Y

app :: Access IO ()
app = do
  A.spawn_ (entity (X 0) <&> Y 1)

  q <- A.all @_ @(Entity '[X, Y])
  liftIO $ pPrint q

  return ()

main :: IO ()
main = do
  _ <- A.runAccess app W.empty
  return ()
