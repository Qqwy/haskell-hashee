module Hashee.Hashed where
import Data.Hashable (Hashable)
import Data.Hashable qualified as Hashable

import Hashee (Hashee)
import Hashee qualified
import Hashee.HashingAlgorithm (HashingAlgorithm(Digest))
import Data.Word (Word64)

data Hashed alg a = Hashed a {-# UNPACK #-} !Word64

-- | Uses precomputed hash to detect inequality faster
instance Eq a => Eq (Hashed alg a) where
  Hashed a ha == Hashed b hb = ha == hb && a == b

instance Ord a => Ord (Hashed alg a) where
  Hashed a _ `compare` Hashed b _ = a `compare` b

instance Show a => Show (Hashed alg a) where
  showsPrec d (Hashed a _) = showParen (d > 10) $
    showString "hashed" . showChar ' ' . showsPrec 11 a

instance Eq a => Hashable (Hashed alg a) where
  hashWithSalt = Hashable.defaultHashWithSalt
  hash (Hashed _ ha) = fromIntegral ha

hashed :: (Hashee a, HashingAlgorithm h, Digest h ~ Word64) => h -> a -> Hashed h a
hashed alg val = let h = Hashee.hash alg val in Hashed val h

