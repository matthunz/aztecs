{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aztecs.Entity
import Data.Aztecs.World
import Text.Pretty.Simple

main :: IO ()
main = do
  let w = world @'[Bool]
      w' = spawn (entity True) w
  pPrint w'
