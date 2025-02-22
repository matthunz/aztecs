{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Aztecs.Input
  ( Key (..),
    InputMotion (..),
    KeyboardInput (..),
    keyboardInput,
    isKeyPressed,
    wasKeyPressed,
    wasKeyReleased,
    handleKeyboardEvent,
    MouseButton (..),
    MouseInput (..),
    mouseInput,
    handleMouseMotion,
  )
where

import Aztecs.ECS
import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Linear (V2 (..))
import Linear.Affine (Point (..))

data Key
  = KeyA
  | KeyB
  | KeyC
  | KeyD
  | KeyE
  | KeyF
  | KeyG
  | KeyH
  | KeyI
  | KeyJ
  | KeyK
  | KeyL
  | KeyM
  | KeyN
  | KeyO
  | KeyP
  | KeyQ
  | KeyR
  | KeyS
  | KeyT
  | KeyU
  | KeyV
  | KeyW
  | KeyX
  | KeyY
  | KeyZ
  | Key0
  | Key1
  | Key2
  | Key3
  | Key4
  | Key5
  | Key6
  | Key7
  | Key8
  | Key9
  | KeyF1
  | KeyF2
  | KeyF3
  | KeyF4
  | KeyF5
  | KeyF6
  | KeyF7
  | KeyF8
  | KeyF9
  | KeyF10
  | KeyF11
  | KeyF12
  | KeyEscape
  | KeyEnter
  | KeySpace
  | KeyBackspace
  | KeyTab
  | KeyCapsLock
  | KeyShift
  | KeyCtrl
  | KeyAlt
  | KeyLeft
  | KeyRight
  | KeyUp
  | KeyDown
  | KeyHome
  | KeyEnd
  | KeyPageUp
  | KeyPageDown
  | KeyInsert
  | KeyDelete
  | KeyMinus
  | KeyEquals
  | KeyBracketLeft
  | KeyBracketRight
  | KeyBackslash
  | KeySemicolon
  | KeyComma
  | KeyPeriod
  | KeySlash
  | KeyNumLock
  | KeyNumpad0
  | KeyNumpad1
  | KeyNumpad2
  | KeyNumpad3
  | KeyNumpad4
  | KeyNumpad5
  | KeyNumpad6
  | KeyNumpad7
  | KeyNumpad8
  | KeyNumpad9
  | KeyNumpadDivide
  | KeyNumpadMultiply
  | KeyNumpadMinus
  | KeyNumpadPlus
  | KeyNumpadEnter
  | KeyNumpadPeriod
  | KeySuper
  | KeyMenu
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, NFData)

-- | Keyboard input component.
data KeyboardInput = KeyboardInput
  { -- | Keyboard events that occured this frame.
    keyboardEvents :: !(Map Key InputMotion),
    -- | Keys that are currently pressed.
    keyboardPressed :: !(Set Key)
  }
  deriving (Show, Generic, NFData)

instance Component KeyboardInput

keyboardInput :: KeyboardInput
keyboardInput = KeyboardInput Map.empty Set.empty

data InputMotion = Pressed | Released
  deriving (Show, Eq, Generic, NFData)

-- | @True@ if this key is currently pressed.
isKeyPressed :: Key -> KeyboardInput -> Bool
isKeyPressed key kb = Set.member key $ keyboardPressed kb

-- | Check for a key event that occured this frame.
keyEvent :: Key -> KeyboardInput -> Maybe InputMotion
keyEvent key kb = Map.lookup key $ keyboardEvents kb

-- | @True@ if this key was pressed this frame.
wasKeyPressed :: Key -> KeyboardInput -> Bool
wasKeyPressed key kb = case keyEvent key kb of
  Just Pressed -> True
  _ -> False

-- | @True@ if this key was released this frame.
wasKeyReleased :: Key -> KeyboardInput -> Bool
wasKeyReleased key kb = case keyEvent key kb of
  Just Released -> True
  _ -> False

handleKeyboardEvent :: Key -> InputMotion -> KeyboardInput -> KeyboardInput
handleKeyboardEvent key motion kb =
  KeyboardInput
    { keyboardEvents = Map.insert key motion $ keyboardEvents kb,
      keyboardPressed = case motion of
        Pressed -> Set.insert key $ keyboardPressed kb
        Released -> Set.delete key $ keyboardPressed kb
    }

data MouseButton
  = ButtonLeft
  | ButtonMiddle
  | ButtonRight
  | ButtonX1
  | ButtonX2
  | -- | An unknown mouse button.
    ButtonExtra !Int
  deriving (Eq, Ord, Show, Generic, NFData)

-- | Mouse input component.
data MouseInput = MouseInput
  { -- | Mouse position in screen-space.
    mousePosition :: !(Point V2 Int),
    -- | Mouse offset since last frame.
    mouseOffset :: !(V2 Int),
    -- | Mouse button states.
    mouseButtons :: !(Map MouseButton InputMotion)
  }
  deriving (Show, Generic, NFData)

instance Component MouseInput

mouseInput :: MouseInput
mouseInput = MouseInput (P 0) (V2 0 0) Map.empty

handleMouseMotion :: V2 Int -> MouseInput -> MouseInput
handleMouseMotion delta mouse =
  mouse
    { mouseOffset = delta,
      mousePosition = mousePosition mouse + P delta
    }
