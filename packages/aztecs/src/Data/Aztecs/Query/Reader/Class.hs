module Data.Aztecs.Query.Reader.Class (ArrowQueryReader (..)) where

import Control.Arrow (Arrow (..), (>>>))
import Data.Aztecs.Component
import Data.Aztecs.Entity (EntityID)

class (Arrow arr) => ArrowQueryReader arr where
  -- | Fetch the currently matched `EntityID`.
  entity :: arr () EntityID

  -- | Fetch a `Component` by its type.
  fetch :: (Component a) => arr () a

  -- | Fetch a `Component` by its type, returning `Nothing` if it doesn't exist.
  fetchMaybe :: (Component a) => arr () (Maybe a)
  fetchMaybe = fetch >>> arr Just
