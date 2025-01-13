{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aztecs.Entity where

import Data.Kind (Type)

data Entity (as :: [Type]) where
  ENil :: Entity '[]
  ECons :: a -> Entity as -> Entity (a ': as)

instance Show (Entity '[]) where
  show ENil = "[]"

instance (Show a, ShowEntity (Entity as)) => Show (Entity (a ': as)) where
  show (ECons x xs) = "[ " ++ show x ++ showEntity xs

class ShowEntity a where
  showEntity :: a -> String

instance ShowEntity (Entity '[]) where
  showEntity ENil = "]"

instance (Show a, ShowEntity (Entity as)) => ShowEntity (Entity (a ': as)) where
  showEntity (ECons x xs) = ", " ++ show x ++ showEntity xs
