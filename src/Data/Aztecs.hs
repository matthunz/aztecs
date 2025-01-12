{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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

entity :: a -> Entity '[a]
entity = flip ECons ENil

data Archetype (ts :: [Type]) where
  ANil :: Archetype '[]
  ACons :: Vector t -> Archetype ts -> Archetype (t ': ts)

instance Show (Archetype '[]) where
  show ANil = "[]"

instance (Show t, ShowArchetype (Archetype ts)) => Show (Archetype (t ': ts)) where
  show (ACons x xs) = "[" ++ show x ++ showArchetype xs

class ShowArchetype a where
  showArchetype :: a -> String

instance ShowArchetype (Archetype '[]) where
  showArchetype ANil = "]"

instance (Show t, ShowArchetype (Archetype ts)) => ShowArchetype (Archetype (t ': ts)) where
  showArchetype (ACons x xs) = ", " ++ show x ++ showArchetype xs

arch :: [a] -> Archetype '[a]
arch as = ACons (V.fromList as) ANil

(<&>) :: Archetype ts -> [t] -> Archetype (t : ts)
(<&>) a v = ACons (V.fromList v) a

class Has a l where
  component :: l -> a
  setComponent :: a -> l -> l

instance {-# OVERLAPPING #-} Has a (Entity (a ': ts)) where
  component (ECons x _) = x
  setComponent x (ECons _ xs) = ECons x xs

instance {-# OVERLAPPING #-} (Has a (Entity ts)) => Has a (Entity (b ': ts)) where
  component (ECons _ xs) = component xs
  setComponent x (ECons y xs) = ECons y (setComponent x xs)

instance {-# OVERLAPPING #-} Has [a] (Archetype (a ': ts)) where
  component (ACons v _) = V.toList v
  setComponent v (ACons _ xs) = ACons (V.fromList v) xs

instance {-# OVERLAPPING #-} (Has [a] (Archetype ts)) => Has [a] (Archetype (b ': ts)) where
  component (ACons _ xs) = component xs
  setComponent v (ACons x xs) = ACons x (setComponent v xs)

class Match (as :: [Type]) (es :: [Type]) where
  match :: Archetype as -> [Entity es]
  alter :: [Entity es] -> Archetype as -> Archetype as

instance Match as '[] where
  match _ = []
  alter _ as = as

instance {-# OVERLAPS #-} (Has [e] (Archetype as)) => Match as '[e] where
  match as =
    let es = component as
        f e = ECons e ENil
     in fmap f es
  alter es as =
    let (es', es'') = unzip $ fmap (\(ECons e t) -> (e, t)) es
        as' = setComponent es' as
     in alter es'' as'

instance forall as es e. (Match as es, Has [e] (Archetype as)) => Match as (e ': es) where
  match as =
    let es = component as
        entities = match as
        f (e, y) = ECons e y
     in fmap f (zip es entities)
  alter es as =
    let (es', es'') = unzip $ fmap (\(ECons e t) -> (e, t)) es
        as' = setComponent es' as
     in alter es'' as'

query :: (Match as bs) => Archetype as -> [Entity bs]
query = match

map :: (Match as bs) => Archetype as -> (Entity bs -> Entity bs) -> Archetype as
map a f =
  let es = match a
      es' = fmap f es
   in alter es' a
