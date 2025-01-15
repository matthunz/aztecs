{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aztecs

main :: IO ()
main = do
  let e = entity (42 :: Int) <&> "Hello, World!"
      a = toArchetype e
      x = getComponentDyn @Int e
  print x
