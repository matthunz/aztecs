{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Table
  ( ColumnID (..),
    RowID (..),
    Column (..),
    Table (..),
    singleton,
    insert,
    remove,
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

insert :: (Typeable a) => ColumnID -> RowID -> a -> Table -> Table
insert (ColumnID colId) (RowID rowId) d (Table table) =
  let h v = MV.write v colId d
      g d' =
        let (Column col) = fromMaybe (error "TODO") $ fromDynamic d'
         in toDyn . Column $ V.modify h col
      f :: MV.MVector s Dynamic -> ST s ()
      f v = MV.modify v g rowId
   in Table $ V.modify f table

remove :: (Typeable a) => ColumnID -> RowID -> Table -> (a, Table)
remove (ColumnID c) (RowID r) (Table v) = case fromDynamic (v V.! r) of
  Just (Column col) ->
    let a = col V.! c
        col' = V.modify (\v -> MV.write v c undefined) col
        v' = V.modify (\v -> MV.write v r (toDyn $ Column col')) v
     in (a, Table v')
  Nothing -> error "TODO"
