{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

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
  return ()

main :: IO ()
main = do
  (_, w) <- A.runAccess app W.empty
  pPrint w
