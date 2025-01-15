{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aztecs.Row where

data Row f (as :: [k]) where
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

class Has a c where
  get :: c -> a

instance {-# OVERLAPPING #-} Has (f a) (Row f (a ': ts)) where
  get (Cons x _) = x

instance {-# OVERLAPPING #-} (Has (f a) (Row f ts)) => Has (f a) (Row f (b ': ts)) where
  get (Cons _ xs) = get xs
