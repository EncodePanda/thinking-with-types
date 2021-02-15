{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Twt.Ch5 where

import Data.Kind (Constraint, Type)

data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)
infixr 5 :#

hLength :: HList ts -> Int
hLength HNil = 0
hLength (_ :# ts)= 1 + hLength ts

hHead :: HList (t ': ts) -> t
hHead (t :# _) = t

instance Show (HList '[]) where
  show _ = "[]"

instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
  show (t :# ts) = show t ++ ", " ++ (show ts)
