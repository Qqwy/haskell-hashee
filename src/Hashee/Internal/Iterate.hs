-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -O2 -ddump-stg-from-core #-}
module Hashee.Internal.Iterate where

import GHC.TypeLits
import Data.Typeable (Proxy(Proxy))

-- -- Implementation with thanks to Mango IV!
-- class Iterate (n :: Nat) where
--   iter :: (a -> a) -> (a -> a)

-- instance {-# OVERLAPPING #-} Iterate 0 where
--   {-# INLINE iter #-}
--   iter _ = id 

-- instance Iterate (n - 1) => Iterate n where 
--   {-# INLINE iter #-}
--   iter f = f . iter @(n - 1) f

iter :: forall n a. (KnownNat n) => (a -> a) -> a -> a
iter f z = iter' f z (natVal (Proxy @n))
  where
    iter' g !x = \case
      0 -> x
      m -> iter' g (g x) (m - 1)
