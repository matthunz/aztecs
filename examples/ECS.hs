{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Aztecs.World as W
import Text.Pretty.Simple
import Data.Aztecs.Entity

newtype X = X Int deriving (Eq, Show)

instance Component X

main :: IO ()
main = do
  let w = W.spawn (X 0) W.empty
  pPrint w
