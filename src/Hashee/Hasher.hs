{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
module Hashee.Hasher where

import Hashee.HasherState (HasherState)
import Hashee.HasherState qualified as HasherState
import Data.Primitive (Prim)
import Data.Primitive qualified as Primitive
import Data.Primitive.ByteArray (ByteArray(ByteArray))
import Data.Primitive.ByteArray qualified as ByteArray
import Data.Word
import Data.Int
import Control.Monad.ST (runST)

newtype Hasher = Hasher (forall hr. HasherState hr => hr -> hr)

instance Semigroup Hasher where
    (Hasher f) <> (Hasher g) = Hasher (g . f)

instance Monoid Hasher where
    mempty = Hasher id

bytes :: ByteArray -> Hasher
bytes !ba = Hasher (HasherState.updateBytes ba)

word :: Word -> Hasher
word !x = Hasher (HasherState.updateWord64 (fromIntegral x))

word64 :: Word64 -> Hasher
word64 !x = Hasher (HasherState.updateWord64 x)

word32 :: Word32 -> Hasher
word32 !x = Hasher (HasherState.updateWord32 x)

word16 :: Word16 -> Hasher
word16 !x = Hasher (HasherState.updateWord16 x)

word8 :: Word8 -> Hasher
word8 !x = Hasher (HasherState.updateWord8 x)

int :: Int -> Hasher
int !x = Hasher (HasherState.updateInt64 (fromIntegral x))

int64 :: Int64 -> Hasher
int64 !x = Hasher (HasherState.updateInt64 x)

int32 :: Int32 -> Hasher
int32 !x = Hasher (HasherState.updateInt32 x)

int16 :: Int16 -> Hasher
int16 !x = Hasher (HasherState.updateInt16 x)

int8 :: Int8 -> Hasher
int8 !x = Hasher (HasherState.updateInt8 x)
