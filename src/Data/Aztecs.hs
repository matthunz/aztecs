module Data.Aztecs (EntityID (..), Component) where

import Data.Data (Typeable)

-- | EntityIDID.
newtype EntityID = EntityID {unEntityId :: Int}
  deriving (Eq, Ord, Show)

class (Typeable a) => Component a
