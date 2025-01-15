{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aztecs

main :: IO ()
main = do
  let e = entity (42 :: Int) <&> "Hello, World!"
      (eId, w) = spawn e world
      x = getComponentDyn @Int e
  print x
