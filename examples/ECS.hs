{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aztecs.Entity
import qualified Data.Aztecs.World as W
import Text.Pretty.Simple

newtype X = X Int deriving (Eq, Show)

instance Component X

newtype Y = Y Int deriving (Eq, Show)

instance Component Y

main :: IO ()
main = do
  let (e, w) = W.spawn (X 0) W.empty
      w' = W.insert e (Y 1) w
  pPrint w'
