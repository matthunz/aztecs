{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aztecs
  ( EntityID (..),
    Entity (..),
    entity,
    (<&>),
    getComponentDyn,
    Archetype (..),
  )
where

import Control.Monad.Identity (Identity (Identity))
import Data.Data (TypeRep, Typeable)
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable (Proxy (..), typeOf)
import Data.Vector (Vector)
import GHC.Base (Any)
import Unsafe.Coerce (unsafeCoerce)

newtype EntityID = EntityID {unEntityID :: Int}
  deriving (Eq, Ord, Show)

data Row f (as :: [Type]) where
  Nil :: Row f '[]
  Cons :: f a -> Row f as -> Row f (a ': as)

instance Show (Row f '[]) where
  show Nil = "[]"

instance (Show (f a), Show' (Row f as)) => Show (Row f (a ': as)) where
  show (Cons x xs) = "[ " ++ show x ++ showRow xs

class Show' a where
  showRow :: a -> String

instance Show' (Row f '[]) where
  showRow Nil = "]"

instance (Show (f a), Show' (Row f as)) => Show' (Row f (a ': as)) where
  showRow (Cons x xs) = ", " ++ show x ++ showRow xs

instance {-# OVERLAPPING #-} Has (f a) (Row f (a ': ts)) where
  get (Cons x _) = x

instance {-# OVERLAPPING #-} (Has (f a) (Row f ts)) => Has (f a) (Row f (b ': ts)) where
  get (Cons _ xs) = get xs

newtype EntityRow as = EntityRow {unEntityRow :: Row Identity as}

instance (Has (Identity a) (Row Identity as)) => Has a (EntityRow as) where
  get e = let (Identity a) = get (unEntityRow e) in a

data Entity as = Entity
  { entityRow :: EntityRow as,
    getters :: Map TypeRep (EntityRow as -> Any)
  }

class Has a c where
  get :: c -> a

entity :: forall a. (Typeable a, Has a (EntityRow '[a])) => a -> Entity '[a]
entity a =
  Entity
    { entityRow = EntityRow $ Cons (Identity a) Nil,
      getters = Map.singleton (typeOf (Proxy @a)) (\e -> unsafeCoerce (get @a e))
    }

(<&>) ::
  forall a as.
  (Typeable a, Has a (EntityRow (a ': as))) =>
  Entity as ->
  a ->
  Entity (a ': as)
(<&>) e a =
  Entity
    { entityRow = EntityRow $ Cons (Identity a) (unEntityRow $ entityRow e),
      getters =
        Map.insert
          (typeOf (Proxy @a))
          (\e' -> unsafeCoerce (get @a e'))
          (fmap (\f -> \(EntityRow (Cons _ es)) -> f (EntityRow es)) (getters e))
    }

getComponentDyn :: forall a as. (Typeable a) => Entity as -> Maybe a
getComponentDyn e = do
  f <- Map.lookup (typeOf (Proxy @a)) (getters e)
  return . unsafeCoerce $ f (entityRow e)

newtype Archetype as = Archetype {unArchetype :: Row Vector as}
