{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aztecs where

class Has a l where
  component :: l -> a
  setComponent :: a -> l -> l

newtype EntityID = EntityID {unEntityId :: Int} deriving (Show)
