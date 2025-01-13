{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aztecs.Entity
import Data.Aztecs.World
import Text.Pretty.Simple

main :: IO ()
main = do
  let w = world @'[Bool, Int]
      (e, w') = spawn (entity (0 :: Int) <&> True) w
      x = Data.Aztecs.World.lookup @'[Bool, Int] e w'
  pPrint x
