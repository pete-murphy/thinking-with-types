{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE KindSignatures #-}

-- {-# LANGUAGE StarIsType #-}

import Data.Functor.Contravariant (Contravariant (..))
import Data.Kind (Type)

class Invariant (f :: Type -> Type) where
  invmap :: (a -> b) -> (b -> a) -> f a -> f b

newtype T1 a = T1 (Int -> a)

instance Functor T1 where
  fmap f (T1 g) = T1 (f . g)

newtype T2 a = T2 (a -> Int)

instance Contravariant T2 where
  contramap f (T2 g) = T2 (g . f)

newtype T3 a = T3 (a -> a)

instance Invariant T3 where
  invmap to from (T3 f) = T3 (to . f . from)

newtype T4 a = T4 ((Int -> a) -> Int)

instance Contravariant T4 where
  contramap f (T4 g) = T4 \h -> g (f . h)

newtype T5 a = T5 ((a -> Int) -> Int)

instance Functor T5 where
  fmap f (T5 g) = T5 \h -> g (h . f)
