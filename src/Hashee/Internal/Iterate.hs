{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -O2 -ddump-stg-from-core #-}
module Hashee.Internal.Iterate where
    
import Data.Monoid (Endo(..))
import GHC.TypeLits

-- Implementation with thanks to Mango IV!
class Iterate (n :: Nat) where
  iter :: (a -> a) -> (a -> a)

instance {-# OVERLAPPING #-} Iterate 0 where
  {-# INLINE iter #-}
  iter _ = id 

instance Iterate (n - 1) => Iterate n where 
  {-# INLINE iter #-}
  iter f = f . iter @(n - 1) f

