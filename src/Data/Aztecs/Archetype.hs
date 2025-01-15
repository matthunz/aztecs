{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aztecs.Archetype where

import Data.Aztecs.Entity (Entity (..), EntityColumn (..), EntityRow (..), entity, (<&>))
import Data.Aztecs.Row (Has (..), Row (..))
import Data.Data (Proxy (..), Typeable, typeOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
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
    access <- Map.lookup (typeOf (Proxy @a)) (archetypeAccess a)
    e <- unsafeCoerce $ (archetypeAccessGetter access) i (archetypeRow a)
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
    access <- Map.lookup (typeOf (Proxy @a)) (archetypeAccess a)
    e <- unsafeCoerce $ (archetypeAccessGetter access) i (archetypeRow a)
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

data ArchetypeAccess as = ArchetypeAccess
  { archetypeAccessGetter :: Int -> ArchetypeRow as -> Maybe Any,
    archetypeAccessQuery :: ArchetypeRow as -> Vector Any
  }

data Archetype as = Archetype
  { archetypeRow :: ArchetypeRow as,
    archetypeAccess :: Map TypeRep (ArchetypeAccess as)
  }

class Empty a where
  empty :: a

instance Empty (Archetype '[]) where
  empty = Archetype {archetypeRow = ArchetypeRow Nil, archetypeAccess = Map.empty}

instance (Empty (Archetype as)) => Empty (Archetype (a ': as)) where
  empty =
    let next = empty
     in Archetype
          { archetypeRow = ArchetypeRow (Cons V.empty (unArchetypeRow $ archetypeRow next)),
            archetypeAccess = Map.empty
          }

class ToArchetype as a where
  toArchetype' :: a -> Archetype as

instance ToArchetype '[] (EntityRow '[]) where
  toArchetype' _ = Archetype {archetypeRow = ArchetypeRow Nil, archetypeAccess = Map.empty}

instance
  (Typeable a, ToArchetype as (EntityRow as)) =>
  ToArchetype (a : as) (EntityRow (a : as))
  where
  toArchetype' (EntityRow (Cons (EntityColumn e) es)) =
    let Archetype (ArchetypeRow nextRow) access = toArchetype' (EntityRow es)
     in Archetype
          { archetypeRow = ArchetypeRow $ Cons (pure e) nextRow,
            archetypeAccess =
              Map.insert
                (typeOf (Proxy @a))
                ( ArchetypeAccess
                    { archetypeAccessQuery = \(ArchetypeRow (Cons es' _)) -> unsafeCoerce <$> es',
                      archetypeAccessGetter = \i (ArchetypeRow (Cons es' _)) -> Just (unsafeCoerce (es' V.! i))
                    }
                )
                ( fmap
                    ( \a ->
                        a
                          { archetypeAccessQuery = \(ArchetypeRow (Cons _ es')) ->
                              (archetypeAccessQuery a) (ArchetypeRow es'),
                            archetypeAccessGetter = \i (ArchetypeRow (Cons _ es')) ->
                              (archetypeAccessGetter a) i (ArchetypeRow es')
                          }
                    )
                    access
                )
          }

toArchetype :: (ToArchetype as (EntityRow as)) => Entity as -> Archetype as
toArchetype = toArchetype' . entityRow

class ToTypeSet a where
  toTypeSet :: Proxy a -> Set TypeRep

instance ToTypeSet (Archetype '[]) where
  toTypeSet _ = Set.empty

instance (Typeable a, ToTypeSet (Archetype as)) => ToTypeSet (Archetype (a ': as)) where
  toTypeSet _ = Set.insert (typeOf (Proxy @a)) (toTypeSet (Proxy @(Archetype as)))

class Query a where
  query :: AnyArchetype -> [a]

instance Query (Entity '[]) where
  query _ = []

instance {-# OVERLAPPING #-} (Typeable a) => Query (Entity '[a]) where
  query (AnyArchetype a) =
    let res = do
          access <- Map.lookup (typeOf (Proxy @a)) (archetypeAccess a)
          let v = (archetypeAccessQuery access) (archetypeRow a)
          return $ map (entity . unsafeCoerce) (V.toList v)
     in fromMaybe [] res

instance (Typeable a, Query (Entity as)) => Query (Entity (a ': as)) where
  query (AnyArchetype a) =
    let res = do
          access <- Map.lookup (typeOf (Proxy @a)) (archetypeAccess a)
          let v = (archetypeAccessQuery access) (archetypeRow a)
          return $ map (unsafeCoerce @_ @a) (V.toList v)
        es = fromMaybe [] res
        es' = query @(Entity as) (AnyArchetype a)
        f (e, es'') = es'' <&> e
     in map f (zip es es')

data AnyArchetype = forall as. AnyArchetype (Archetype as)
