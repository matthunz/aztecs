{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aztecs.Archetype where

import Data.Aztecs
import Data.Aztecs.Entity
import Data.Kind (Type)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Prelude hiding (lookup)

data Archetype (as :: [Type]) where
  ANil :: Archetype '[]
  ACons :: Vector a -> Archetype as -> Archetype (a ': as)

instance Show (Archetype '[]) where
  show ANil = "[]"

instance (Show a, ShowArchetype (Archetype as)) => Show (Archetype (a ': as)) where
  show (ACons x xs) = "[ " ++ show x ++ showArchetype xs

class ShowArchetype a where
  showArchetype :: a -> String

instance ShowArchetype (Archetype '[]) where
  showArchetype ANil = "]"

instance (Show a, ShowArchetype (Archetype as)) => ShowArchetype (Archetype (a ': as)) where
  showArchetype (ACons x xs) = ", " ++ show x ++ showArchetype xs

type family And (x :: Bool) (y :: Bool) :: Bool where
  And 'True 'True = 'True
  And _ _ = 'False

type family Subset (es :: [Type]) (as :: [Type]) :: Bool where
  Subset '[] as = 'True
  Subset (e ': es) as = And (Elem e as) (Subset es as)

type family Elem (x :: k) (xs :: [k]) :: Bool where
  Elem _ '[] = 'False
  Elem x (x ': xs) = 'True
  Elem x (_ ': xs) = Elem x xs

class Match (as :: [Type]) (es :: [Type]) where
  match :: Archetype as -> [Entity es]
  alter :: [Entity es] -> Archetype as -> Archetype as
  lookup' :: Int -> Archetype as -> Maybe (Entity es)

instance Match as '[] where
  match _ = []
  alter _ as = as
  lookup' _ _ = Nothing

instance {-# OVERLAPPING #-}
  (Subset '[e] as ~ 'True, Has (Vector e) (Archetype as)) =>
  Match as '[e]
  where
  match as =
    let es = V.toList $ component as
        f e = ECons e ENil
     in fmap f es

  alter es as =
    let (es', es'') = unzip $ fmap (\(ECons e t) -> (e, t)) es
        as' = setComponent (V.fromList es') as
     in alter es'' as'

  lookup' i as = do
    e <- component as V.!? i
    return $ ECons e ENil

instance 
  (Subset es as ~ 'True, Has (Vector e) (Archetype as), Match as es) =>
  Match as (e ': es)
  where
  match as =
    let es = V.toList $ component as
        entities = match as
        f (e, y) = ECons e y
     in fmap f (zip es entities)

  alter es as =
    let (es', es'') = unzip $ fmap (\(ECons e t) -> (e, t)) es
        as' = setComponent (V.fromList es') as
     in alter es'' as'

  lookup' i as = do
    e <- component as V.!? i
    es <- lookup' i as
    return $ ECons e es

instance {-# OVERLAPPING #-} Has (Vector a) (Archetype (a ': ts)) where
  component (ACons v _) = v
  setComponent v (ACons _ xs) = ACons v xs

instance {-# OVERLAPPING #-} (Has (Vector a) (Archetype ts)) => Has (Vector a) (Archetype (b ': ts)) where
  component (ACons _ xs) = component xs
  setComponent v (ACons x xs) = ACons x (setComponent v xs)

map :: (Match as bs) => Archetype as -> (Entity bs -> Entity bs) -> Archetype as
map a f =
  let es = match a
      es' = fmap f es
   in alter es' a

lookup ::
  forall (es :: [Type]) (as :: [Type]).
  (Match as es) =>
  Int ->
  Archetype as ->
  Maybe (Entity es)
lookup = lookup'