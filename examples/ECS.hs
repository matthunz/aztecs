{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aztecs.Entity
import Data.Aztecs.World
import Text.Pretty.Simple

main :: IO ()
main = do
  let w = world @'[Bool]
      (e, w') = spawn @_ @'[Bool] (entity True) w
      x = Data.Aztecs.World.lookup @'[Bool] e w'
  pPrint x
