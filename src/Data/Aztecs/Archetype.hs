{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MonoLocalBinds #-}

module Data.Aztecs.Archetype where

import Control.Monad.Identity (Identity (..))
import Data.Aztecs.Entity (Entity (..), EntityRow (..), entity)
import Data.Aztecs.Row (Has (..), Row (..))
import Data.Data (Proxy (..), Typeable, typeOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable (TypeRep)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Base (Any)
import Unsafe.Coerce (unsafeCoerce)

class FromAnyArchetype a where
  fromAnyArchetype :: Int -> AnyArchetype -> Maybe a

instance FromAnyArchetype (Entity '[]) where
  fromAnyArchetype _ _ = Nothing

instance {-# OVERLAPPING #-} (Typeable a) => FromAnyArchetype (Entity '[a]) where
  fromAnyArchetype i (AnyArchetype a) = do
    f <- Map.lookup (typeOf (Proxy @a)) (archetypeGetters a)
    e <- unsafeCoerce $ f i (archetypeRow a)
    let e' = entity (unsafeCoerce e)
        EntityRow (Cons x Nil) = entityRow e'
    return $
      ( e'
          { entityRow = EntityRow $ Cons x Nil,
            getters =
              Map.singleton
                (typeOf (Proxy @a))
                (\e'' -> unsafeCoerce (get @a e''))
          }
      )

instance (Typeable a, FromAnyArchetype (Entity as)) => FromAnyArchetype (Entity (a ': as)) where
  fromAnyArchetype i (AnyArchetype a) = do
    f <- Map.lookup (typeOf (Proxy @a)) (archetypeGetters a)
    e <- unsafeCoerce $ f i (archetypeRow a)
    let e' = entity (unsafeCoerce e)
        EntityRow (Cons x Nil) = entityRow e'
    Entity (EntityRow row') gs <- fromAnyArchetype @(Entity as) i (AnyArchetype a)
    return $
      ( e'
          { entityRow = EntityRow $ Cons x row',
            getters =
              Map.insert
                (typeOf (Proxy @a))
                (\e'' -> unsafeCoerce (get @a e''))
                (fmap (\g -> \(EntityRow (Cons _ es)) -> g (EntityRow es)) gs)
          }
      )

newtype ArchetypeRow as = ArchetypeRow {unArchetypeRow :: Row Vector as}

data Archetype as = Archetype
  { archetypeRow :: ArchetypeRow as,
    archetypeGetters :: Map TypeRep (Int -> ArchetypeRow as -> Maybe Any)
  }

class Empty a where
  empty :: a

instance Empty (Archetype '[]) where
  empty = Archetype {archetypeRow = ArchetypeRow Nil, archetypeGetters = Map.empty}

instance (Empty (Archetype as)) => Empty (Archetype (a ': as)) where
  empty =
    let next = empty
     in Archetype {archetypeRow = ArchetypeRow (Cons V.empty (unArchetypeRow $ archetypeRow next)), archetypeGetters = Map.empty}

class ToArchetype as a where
  toArchetype' :: a -> Archetype as

instance ToArchetype '[] (EntityRow '[]) where
  toArchetype' _ = Archetype {archetypeRow = ArchetypeRow Nil, archetypeGetters = Map.empty}

instance
  (Typeable a, ToArchetype as (EntityRow as)) =>
  ToArchetype (a : as) (EntityRow (a : as))
  where
  toArchetype' (EntityRow (Cons (Identity e) es)) =
    let Archetype (ArchetypeRow nextRow) nextGs = toArchetype' (EntityRow es)
     in Archetype
          { archetypeRow = ArchetypeRow $ Cons (pure e) nextRow,
            archetypeGetters =
              Map.insert
                (typeOf (Proxy @a))
                (\i (ArchetypeRow (Cons v _)) -> unsafeCoerce <$> v V.!? i)
                (fmap (\f -> \i (ArchetypeRow (Cons _ es')) -> f i (ArchetypeRow es')) nextGs)
          }

toArchetype :: (ToArchetype as (EntityRow as)) => Entity as -> Archetype as
toArchetype = toArchetype' . entityRow

newtype ArchetypesRow as = ArchetypesRow {unArchetypesRow :: Row Archetype as}

data AnyArchetype = forall as. AnyArchetype (Archetype as)
