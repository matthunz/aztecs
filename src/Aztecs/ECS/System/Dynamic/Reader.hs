{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.ECS.System.Dynamic.Reader
  ( DynamicReaderSystem (..),
    ArrowDynamicReaderSystem (..),
    ArrowQueueSystem (..),
    raceDyn,
  )
where

import Aztecs.ECS.Access (Access)
import Aztecs.ECS.Query.Dynamic.Reader (DynamicQueryReader)
import Aztecs.ECS.System.Dynamic.Reader.Class (ArrowDynamicReaderSystem (..))
import Aztecs.ECS.System.Queue (ArrowQueueSystem (..))
import qualified Aztecs.ECS.View as V
import Aztecs.ECS.World (World (..))
import Aztecs.ECS.World.Bundle (Bundle)
import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Control.Parallel (par)
import Prelude hiding (id, (.))

newtype DynamicReaderSystem i o = DynamicReaderSystem
  { -- | Run a dynamic system producing some output
    runReaderSystemDyn :: World -> i -> (o, Access (), DynamicReaderSystem i o)
  }
  deriving (Functor)

instance Category DynamicReaderSystem where
  id = DynamicReaderSystem $ \_ i -> (i, pure (), id)
  DynamicReaderSystem f . DynamicReaderSystem g = DynamicReaderSystem $ \w i ->
    let (b, gAccess, g') = g w i
        (c, fAccess, f') = f w b
     in (c, gAccess >> fAccess, f' . g')

instance Arrow DynamicReaderSystem where
  arr f = DynamicReaderSystem $ \_ i -> (f i, pure (), arr f)
  first (DynamicReaderSystem f) = DynamicReaderSystem $ \w (i, x) ->
    let (a, access, f') = f w i in ((a, x), access, first f')

instance ArrowDynamicReaderSystem DynamicQueryReader DynamicReaderSystem where
  allDyn cIds q = DynamicReaderSystem $ \w i ->
    let !v = V.view cIds $ archetypes w in (V.readAllDyn i q v, pure (), allDyn cIds q)
  filterDyn cIds q f = DynamicReaderSystem $ \w i ->
    let !v = V.filterView cIds f $ archetypes w
     in (V.readAllDyn i q v, pure (), filterDyn cIds q f)

instance ArrowQueueSystem Bundle Access DynamicReaderSystem where
  queue f = DynamicReaderSystem $ \_ i -> let !a = f i in ((), a, queue f)

raceDyn :: DynamicReaderSystem i a -> DynamicReaderSystem i b -> DynamicReaderSystem i (a, b)
raceDyn (DynamicReaderSystem f) (DynamicReaderSystem g) = DynamicReaderSystem $ \w i ->
  let fa = f w i
      gb = g w i
      gbPar = fa `par` gb
      (a, fAccess, f') = fa
      (b, gAccess, g') = gbPar
   in ((a, b), fAccess >> gAccess, raceDyn f' g')
