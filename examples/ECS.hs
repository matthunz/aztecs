{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aztecs
import qualified Data.Aztecs as ECS
import Data.Aztecs.Entity

main :: IO ()
main = do
  let e = entity (42 :: Int) <&> "Hello, World!"
      (eId, w) = spawn e world
      x = ECS.lookup @'[Int] eId w
  print x
