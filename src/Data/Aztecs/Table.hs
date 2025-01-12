{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Table
  ( ColumnID (..),
    RowID (..),
    Column (..),
    Table (..),
    singleton,
    cons,
    insert,
    insertCons,
    remove,
    toList
  )
where

import Control.Monad.ST (ST)
import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

newtype ColumnID = ColumnID {unColumnID :: Int}
  deriving (Eq, Ord, Show)

newtype RowID = RowID {unRowID :: Int}
  deriving (Eq, Ord, Show)

newtype Column a = Column (Vector a)
  deriving (Show)

newtype Table = Table (Vector Dynamic)
  deriving (Show)

singleton :: (Typeable a) => a -> Table
singleton c = Table $ V.singleton (toDyn $ Column $ V.singleton c)

cons :: (Typeable a) => a -> Table -> Table
cons c (Table v) = Table $ V.cons (toDyn $ Column $ V.singleton c) v

insertCons :: (Typeable a) => ColumnID -> a -> Table -> Table
insertCons (ColumnID colId) c (Table table) =
  let g dyn =
        let (Column col) = fromMaybe (error "TODO") $ fromDynamic dyn
         in toDyn . Column $ V.cons c col
      f :: MV.MVector s Dynamic -> ST s ()
      f v = MV.modify v g colId
   in Table $ V.modify f table

insert :: (Typeable a) => ColumnID -> RowID -> a -> Table -> Table
insert (ColumnID colId) (RowID rowId) d (Table table) =
  let h v = MV.write v rowId d
      g d' =
        let (Column col) = fromMaybe (error "TODO") $ fromDynamic d'
         in toDyn . Column $ V.modify h col
      f :: MV.MVector s Dynamic -> ST s ()
      f v = MV.modify v g colId
   in Table $ V.modify f table

remove :: (Typeable a) => ColumnID -> RowID -> Table -> (a, Table)
remove (ColumnID c) (RowID r) (Table v) = case fromDynamic (v V.! r) of
  Just (Column col) ->
    let a = col V.! c
        col' = V.modify (\acc -> MV.write acc c undefined) col
        v' = V.modify (\acc -> MV.write acc r (toDyn $ Column col')) v
     in (a, Table v')
  Nothing -> error "TODO"

toList :: (Typeable a) => ColumnID -> Table -> [a]
toList (ColumnID c) (Table v) =
  map
    ( \(Column col) ->
        col V.! c
    )
    $ map (\dyn -> fromMaybe (error "TODO") $ fromDynamic dyn)
    $ V.toList v