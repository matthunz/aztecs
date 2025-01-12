{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aztecs where

import Data.Kind (Type)
import Data.Vector (Vector)
import qualified Data.Vector as V

data Entity (ts :: [Type]) where
  ENil :: Entity '[]
  ECons :: t -> Entity ts -> Entity (t ': ts)

instance Show (Entity '[]) where
  show ENil = "[]"

instance (Show t, ShowEntity (Entity ts)) => Show (Entity (t ': ts)) where
  show (ECons x xs) = "[" ++ show x ++ showEntity xs

class ShowEntity a where
  showEntity :: a -> String

instance ShowEntity (Entity '[]) where
  showEntity ENil = "]"

instance (Show t, ShowEntity (Entity ts)) => ShowEntity (Entity (t ': ts)) where
  showEntity (ECons x xs) = ", " ++ show x ++ showEntity xs

data Archetype (ts :: [Type]) where
  ANil :: Archetype '[]
  ACons :: Vector t -> Archetype ts -> Archetype (t ': ts)

arch :: [a] -> Archetype '[a]
arch as = ACons (V.fromList as) ANil

class Match (as :: [Type]) (bs :: [Type]) where
  match :: Archetype as -> [Entity bs]

instance Match '[] '[] where
  match ANil = []

instance {-# OVERLAPS #-} Match '[a] '[a] where
  match (ACons v _) =
    let bs = (V.toList v)
        f a = ECons a ENil
     in map f bs

instance (Match as bs) => Match (a ': as) (a ': bs) where
  match (ACons v as) =
    let bs = (V.toList v)
        es = match as
        f (a, b) = ECons a b
     in map f (zip bs es)

query :: (Match as bs) => Archetype as -> [Entity bs]
query = match
