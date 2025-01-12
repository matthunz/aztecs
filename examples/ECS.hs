{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aztecs
import Text.Pretty.Simple

main :: IO ()
main = do
  let a = arch @Int [1, 2, 3] <&> ["abc"]
      b = query @_ @'[String, Int] a
  pPrint b
