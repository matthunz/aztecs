{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aztecs
import Text.Pretty.Simple

main :: IO ()
main = do
  let a = arch @Int [1, 2, 3] <&> ["abc"]
      f x = entity (component x + 1)
      b = Data.Aztecs.map @_ @'[Int] a f
  pPrint b
