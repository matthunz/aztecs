{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow (returnA, (>>>))
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import Data.Aztecs.Asset (load)
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.SDL (Camera (..), Sprite (..), SpriteAnimation (..), Window (..), spriteAnimation)
import qualified Data.Aztecs.SDL as SDL
import qualified Data.Aztecs.System as S
import Data.Aztecs.Transform (Transform (..), transform)
import SDL (Point (..), Rectangle (..), V2 (..))

setup :: System () ()
setup =
  S.mapSingle
    ( proc () -> do
        assetServer <- Q.fetch -< ()
        (texture, assetServer') <- Q.run (load "assets/characters.png") -< assetServer
        Q.set -< assetServer'
        returnA -< texture
    )
    >>> S.queue
      ( \texture -> do
          A.spawn_ $ bundle Window {windowTitle = "Aztecs"}
          A.spawn_ $ bundle Camera {cameraViewport = V2 1000 500} <> bundle transform
          A.spawn_ $
            bundle
              Sprite
                { spriteTexture = texture,
                  spriteSize = V2 300 300,
                  spriteBounds = Just $ Rectangle (P $ V2 0 32) (V2 32 32)
                }
              <> bundle
                spriteAnimation
                  { spriteAnimationSteps =
                      [ Rectangle (P $ V2 576 32) (V2 32 32),
                        Rectangle (P $ V2 608 32) (V2 32 32),
                        Rectangle (P $ V2 640 32) (V2 32 32),
                        Rectangle (P $ V2 672 32) (V2 32 32)
                      ]
                  }
              <> bundle transform {transformPosition = V2 100 100}
      )

update :: System () ()
update = S.all (Q.fetch @_ @SDL.Keyboard) >>> S.run (\keyboard -> print keyboard)

main :: IO ()
main = runSystem_ $ SDL.setup >>> setup >>> S.forever (SDL.update >>> update >>> SDL.draw)
