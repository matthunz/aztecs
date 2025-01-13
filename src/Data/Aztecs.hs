{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aztecs (EntityID (..) , Has(..)) where

-- | EntityIDID.
newtype EntityID = EntityID {unEntityID :: Int}
  deriving (Eq, Ord, Show)

class Has a l where
  component :: l -> a
  setComponent :: a -> l -> l
