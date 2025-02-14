{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow ((>>>))
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.SDL (MouseInput, Window (..))
import qualified Data.Aztecs.SDL as SDL
import qualified Data.Aztecs.System as S

setup :: System () ()
setup = S.queue . const . A.spawn_ $ bundle Window {windowTitle = "Aztecs"}

update :: System () ()
update = S.all (Q.fetch @_ @MouseInput) >>> S.run print

main :: IO ()
main = runSystem_ $ SDL.setup >>> setup >>> S.forever (SDL.update >>> update >>> SDL.draw)
