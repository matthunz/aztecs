{-# LANGUAGE DeriveFunctor #-}

module Data.Aztecs
  ( Entity,
    EntityComponent (..),
    Storage (..),
    table,
    Component (..),
    World,
    newWorld,
    spawn,
    insert,
    get,
    Query,
    read,
    query,
    queryAll,
  )
where

import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Functor ((<&>))
import Data.List (find)
import Data.Map (Map, alter, empty, lookup)
import Data.Maybe (fromMaybe, isJust)
import Data.Typeable
import Prelude hiding (read)

newtype Entity = Entity Int deriving (Eq, Show)

data EntityComponent a = EntityComponent Entity a deriving (Show)

table' :: [EntityComponent a] -> Storage a
table' cs =
  Storage
    { empty' = table' [],
      spawn' = \e a -> table' (EntityComponent e a : cs),
      get' = \e ->
        find (\(EntityComponent e' _) -> e == e') cs
          <&> \(EntityComponent _ a) -> a,
      toList = cs
    }

table :: Storage a
table = table' []

data Storage a = Storage
  { empty' :: Storage a,
    spawn' :: Entity -> a -> Storage a,
    get' :: Entity -> Maybe a,
    toList :: [EntityComponent a]
  }

class Component a where
  storage :: Storage a
  storage = table

data World = World (Map TypeRep Dynamic) Entity deriving (Show)

newWorld :: World
newWorld = World empty (Entity 0)

spawn :: (Component c, Typeable c) => c -> World -> (Entity, World)
spawn = f (Proxy)
  where
    f :: (Component c, Typeable c) => Proxy c -> c -> World -> (Entity, World)
    f p c (World w (Entity e)) =
      ( Entity e,
        World
          ( alter
              ( \maybeRow -> case maybeRow of
                  Just row -> fmap (\row' -> toDyn $ spawn' row' (Entity e) c) (fromDynamic row)
                  Nothing -> Just $ toDyn $ spawn' storage (Entity e) c
              )
              (typeOf p)
              w
          )
          (Entity (e + 1))
      )

insert :: (Component c, Typeable c) => Entity -> c -> World -> World
insert = f Proxy
  where
    f :: (Component c, Typeable c) => Proxy c -> Entity -> c -> World -> World
    f p e c (World w e') =
      World
        ( alter
            ( \maybeRow -> Just $ toDyn $ g (maybeRow >>= fromDynamic) e c
            )
            (typeOf p)
            w
        )
        e'
    g :: (Component c, Typeable c) => Maybe (Storage c) -> Entity -> c -> Storage c
    g maybeRow e c = case maybeRow of
      Just row -> spawn' row e c
      Nothing -> spawn' storage e c

getRow :: (Typeable c) => Proxy c -> World -> Maybe (Storage c)
getRow p (World w _) = Data.Map.lookup (typeOf p) w >>= fromDynamic

get :: (Typeable c) => Entity -> World -> Maybe c
get e (World w _) = f (Proxy)
  where
    f :: (Typeable c) => Proxy c -> Maybe c
    f p = Data.Map.lookup (typeOf p) w >>= fromDynamic >>= flip get' e

data ReadWrites = ReadWrites [TypeRep] [TypeRep]

instance Semigroup ReadWrites where
  ReadWrites rs ws <> ReadWrites rs' ws' = ReadWrites (rs <> rs') (ws <> ws')

instance Monoid ReadWrites where
  mempty = ReadWrites [] []

data Query a = Query ReadWrites (Maybe [Entity] -> World -> ([Entity], [a])) (Entity -> World -> Maybe a)
  deriving (Functor)

instance Applicative Query where
  pure a = Query mempty (\_ _ -> ([], [a])) (\_ _ -> Just a)
  Query rs f g <*> Query rs' f' g' =
    Query
      (rs <> rs')
      ( \es w ->
          let (es1, fs) = f es w
           in case es1 of
                [] -> ([], [])
                _ ->
                  let (es2, as) = f' es w
                   in (es1 <> es2, fs <*> as)
      )
      ( \e w ->
          case g e w of
            Just a -> case g' e w of
              Just a' -> Just $ a a'
              Nothing -> Nothing
            Nothing -> Nothing
      )

read :: (Typeable a) => Query a
read = f Proxy
  where
    f :: (Typeable a) => Proxy a -> Query a
    f p =
      Query
        (ReadWrites [typeOf p] [])
        ( \es w ->
            let row = (fromMaybe [] (fmap toList (getRow p w)))
             in case es of
                  Just es' ->
                    let row' = (filter (\(EntityComponent e _) -> isJust $ find (== e) es') row')
                     in foldr (\(EntityComponent e a) (es'', as) -> (e : es'', a : as)) ([], []) row'
                  Nothing -> (map (\(EntityComponent e _) -> e) row, map (\(EntityComponent _ a) -> a) row)
        )
        (get)

query :: Entity -> Query a -> World -> Maybe a
query e (Query _ _ f) w = f e w

queryAll :: Query a -> World -> [a]
queryAll (Query _ f _) w = snd $ f Nothing w
