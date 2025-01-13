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

module Data.Aztecs.Entity where

import Data.Aztecs
import Data.Kind (Type)

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

instance {-# OVERLAPPING #-} Has a (Entity (a ': ts)) where
  component (ECons x _) = x
  setComponent x (ECons _ xs) = ECons x xs

instance {-# OVERLAPPING #-} (Has a (Entity ts)) => Has a (Entity (b ': ts)) where
  component (ECons _ xs) = component xs
  setComponent x (ECons y xs) = ECons y (setComponent x xs)

entity :: a -> Entity '[a]
entity = flip ECons ENil

(<&>) :: Entity as -> a -> Entity (a : as)
(<&>) es c = ECons c es
