{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aztecs
import qualified Data.Aztecs as ECS
import Data.Aztecs.Entity
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  let e = entity (42 :: Int) <&> "Hello, World!"
      (_, w) = spawn e world
      x = ECS.query @'[String, Int] w
  pPrint w
