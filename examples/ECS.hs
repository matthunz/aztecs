{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aztecs.Archetype
import qualified Data.Aztecs.Archetype as A
import Data.Aztecs.Entity (Entity (ECons, ENil))
import Text.Pretty.Simple

main :: IO ()
main = do
  let x = ACons (pure (1 :: Int)) (ACons (pure True) ANil)
      y = Data.Aztecs.Archetype.map x (\((ECons i ENil) :: Entity '[Int]) -> ECons (i + 1) ENil)
      z = A.lookup @'[Bool] 0 x
  pPrint z
