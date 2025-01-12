{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aztecs.Archetype where

import Data.Aztecs
import Data.Aztecs.Entity (Entity (..))
import Data.Kind (Type)
import Data.Vector (Vector)
import qualified Data.Vector as V

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

instance {-# OVERLAPPING #-} Has [a] (Archetype (a ': ts)) where
  component (ACons v _) = V.toList v
  setComponent v (ACons _ xs) = ACons (V.fromList v) xs

instance {-# OVERLAPPING #-} (Has [a] (Archetype ts)) => Has [a] (Archetype (b ': ts)) where
  component (ACons _ xs) = component xs
  setComponent v (ACons x xs) = ACons x (setComponent v xs)

query :: (Match as bs) => Archetype as -> [Entity bs]
query = match

map :: (Match as bs) => Archetype as -> (Entity bs -> Entity bs) -> Archetype as
map a f =
  let es = match a
      es' = fmap f es
   in alter es' a

(<&>) :: Archetype ts -> [t] -> Archetype (t : ts)
(<&>) a v = ACons (V.fromList v) a
