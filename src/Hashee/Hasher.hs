{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
module Hashee.Hasher where

import Hashee.HashingAlgorithm (HashingAlgorithm(..), ConcreteHasher)
import Hashee.HashingAlgorithm qualified as HashingAlgorithm
import Data.Primitive (Prim)
import Data.Primitive qualified as Primitive
import Data.Primitive.ByteArray (ByteArray(ByteArray))
import Data.Primitive.ByteArray qualified as ByteArray
import Data.Word
import Data.Int
import Control.Monad.ST (runST)

newtype Hasher = Hasher (forall hr. HashingAlgorithm hr => ConcreteHasher hr)

runHasher :: HashingAlgorithm h => h -> Hasher -> Digest h
{-# INLINE runHasher #-}
runHasher !alg (Hasher !hr) = runAlg alg hr


instance Semigroup Hasher where
    (Hasher f) <> (Hasher g) = Hasher (g . f)

instance Monoid Hasher where
    mempty = Hasher id

rawBytes :: ByteArray -> Hasher
{-# INLINE rawBytes #-}
rawBytes !ba = Hasher (HashingAlgorithm.updateBytes ba)

bytes :: ByteArray -> Hasher
{-# INLINE bytes #-}
bytes !ba = Hasher (HashingAlgorithm.updateByteString ba)

word :: Word -> Hasher
{-# INLINE word #-}
word !x = Hasher (HashingAlgorithm.updateWord64 (fromIntegral x))

word64 :: Word64 -> Hasher
{-# INLINE word64 #-}
word64 !x = Hasher (HashingAlgorithm.updateWord64 x)

word32 :: Word32 -> Hasher
{-# INLINE word32 #-}
word32 !x = Hasher (HashingAlgorithm.updateWord32 x)

word16 :: Word16 -> Hasher
{-# INLINE word16 #-}
word16 !x = Hasher (HashingAlgorithm.updateWord16 x)

word8 :: Word8 -> Hasher
{-# INLINE word8 #-}
word8 !x = Hasher (HashingAlgorithm.updateWord8 x)

int :: Int -> Hasher
{-# INLINE int #-}
int !x = Hasher (HashingAlgorithm.updateInt64 (fromIntegral x))

int64 :: Int64 -> Hasher
{-# INLINE int64 #-}
int64 !x = Hasher (HashingAlgorithm.updateInt64 x)

int32 :: Int32 -> Hasher
{-# INLINE int32 #-}
int32 !x = Hasher (HashingAlgorithm.updateInt32 x)

int16 :: Int16 -> Hasher
{-# INLINE int16 #-}
int16 !x = Hasher (HashingAlgorithm.updateInt16 x)

int8 :: Int8 -> Hasher
{-# INLINE int8 #-}
int8 !x = Hasher (HashingAlgorithm.updateInt8 x)
