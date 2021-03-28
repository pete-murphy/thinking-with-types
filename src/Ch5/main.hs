{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Kind (Constraint, Type)

data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)

infixr 5 :#

-- instance Eq (HList '[]) where
--   HNil == HNil = True

-- instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
--   (a :# as) == (b :# bs) = a == b && as == bs

-- instance Ord (HList '[]) where
--   HNil <= HNil = True

-- instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
--   (a :# as) <= (b :# bs) = a <= b && as <= bs

-- instance Show (HList '[]) where
--   show HNil = "HNil"

-- instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
--   show (a :# as) = show a <> " :# " <> show as

type family AllEq (ts :: [Type]) :: Constraint where
  AllEq '[] = () -- This is a *type* with kind *Constraint*
  AllEq (t ': ts) = (Eq t, AllEq ts)

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

instance All Eq ts => Eq (HList ts) where
  HNil == HNil = True
  (a :# as) == (b :# bs) = a == b && as == bs

instance (All Eq ts, All Ord ts) => Ord (HList ts) where
  HNil <= HNil = True
  (a :# as) <= (b :# bs) = a <= b && as <= bs

instance All Show ts => Show (HList ts) where
  show HNil = "HNil"
  show (a :# as) = show a <> " :# " <> show as
