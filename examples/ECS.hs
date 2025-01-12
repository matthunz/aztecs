{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aztecs
import Text.Pretty.Simple

main :: IO ()
main = do
  let w = world
  pPrint w
