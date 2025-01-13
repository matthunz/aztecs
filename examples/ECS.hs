{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aztecs.Archetype
import qualified Data.Aztecs.Archetype as A
import qualified Data.Aztecs.Archetypes as AS
import Data.Aztecs.Entity (Entity (ECons, ENil))
import Text.Pretty.Simple

main :: IO ()
main = do
  let x = ACons (pure (1 :: Int)) (ACons (pure True) ANil)
      z = A.lookup @'[Bool] 0 x
      x' = ACons (pure (2 :: Int)) ANil
      as = AS.ACons x' (AS.ACons x AS.ANil)
      as' =
        AS.map
          (\((ECons i ENil) :: Entity '[Int]) -> ECons (i + 1) ENil)
          as
  pPrint as'
