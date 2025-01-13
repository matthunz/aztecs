{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Aztecs.Archetype
import Text.Pretty.Simple
import Data.Aztecs.Entity (Entity (ECons, ENil))

main :: IO ()
main = do
  let x = ACons (pure (1 :: Int)) (ACons (pure True) ANil)
      y = Data.Aztecs.Archetype.map  x  (\((ECons i ENil) :: Entity '[Int]) -> ECons (i + 1) ENil)
  pPrint y
