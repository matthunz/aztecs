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

(<&>) :: Archetype ts -> [t] -> Archetype (t : ts)
(<&>) a v = ACons (V.fromList v) a

class Has a l where
  component :: l -> a

instance {-# OVERLAPPING #-} Has [a] (Archetype (a ': ts)) where
  component (ACons v _) = V.toList v

instance {-# OVERLAPPING #-} (Has [a] (Archetype ts)) => Has [a] (Archetype (b ': ts)) where
  component (ACons _ xs) = component xs

class Match (as :: [Type]) (es :: [Type]) where
  match :: Archetype as -> [Entity es]

instance Match as '[] where
  match _ = []

instance {-# OVERLAPS #-} (Has [e] (Archetype as)) => Match as '[e] where
  match as =
    let es = component as
        f e = ECons e ENil
     in map f es

instance (Match as es, Has [e] (Archetype as)) => Match as (e ': es) where
  match as =
    let es = component as
        entities = match as
        f (e, y) = ECons e y
     in map f (zip es entities)

query :: (Match as bs) => Archetype as -> [Entity bs]
query = match
