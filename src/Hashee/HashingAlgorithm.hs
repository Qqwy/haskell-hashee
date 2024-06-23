{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
module Hashee.HashingAlgorithm where

import Data.Primitive (Prim)
import Data.Primitive qualified as Primitive
import Data.Primitive.ByteArray (ByteArray(ByteArray))
import Data.Primitive.ByteArray qualified as ByteArray
import Data.Word
import Data.Int
import Control.Monad.ST (runST)

type ConcreteHasher h = (HasherState h -> HasherState h)

class HashingAlgorithm h where
  -- | Intermediate state used by the algorithm
  data HasherState h
  -- | Final value produced by running the algorithm to completion
  type Digest h

  -- | Set up the state, run 
  runAlg :: h -> ConcreteHasher h -> Digest h

  updateBytes :: ByteArray -> ConcreteHasher h

  -- | Updates a string or bytestring, including a length prefix
  -- Can be overridden by hashing algs if they already manage this differently
  updateByteString :: ByteArray -> ConcreteHasher h
  updateByteString !ba = (updateBytes ba . updateInt64 (fromIntegral $ ByteArray.sizeofByteArray ba))

  updateWord64 :: Word64 -> ConcreteHasher h
  {-# INLINE updateWord64 #-}
  updateWord64 !x = updateBytes $ unsafeByteArrayFromListN 1 [x]

  updateWord32 :: Word32 -> ConcreteHasher h
  {-# INLINE updateWord32 #-}
  updateWord32 !x = updateBytes $ unsafeByteArrayFromListN 1 [x]

  updateWord16 :: Word16 -> ConcreteHasher h
  {-# INLINE updateWord16 #-}
  updateWord16 !x = updateBytes $ unsafeByteArrayFromListN 1 [x]

  updateWord8 :: Word8 -> ConcreteHasher h
  {-# INLINE updateWord8 #-}
  updateWord8 !x = updateBytes $ unsafeByteArrayFromListN 1 [x]

  updateInt64 :: Int64 -> ConcreteHasher h
  {-# INLINE updateInt64 #-}
  updateInt64 !x = updateWord64 (fromIntegral x)

  updateInt32 :: Int32 -> ConcreteHasher h
  {-# INLINE updateInt32 #-}
  updateInt32 !x = updateWord32 (fromIntegral x)

  updateInt16 :: Int16 -> ConcreteHasher h
  {-# INLINE updateInt16 #-}
  updateInt16 !x = updateWord16 (fromIntegral x)

  updateInt8 :: Int8 -> ConcreteHasher h
  {-# INLINE updateInt8 #-}
  updateInt8 !x = updateWord8 (fromIntegral x)

-- | Create a 'ByteArray' from a list of a known length. 
-- If the length of the list does not match the given length,
-- we're off to undefined behaviour land
unsafeByteArrayFromListN :: forall a. Prim a => Int -> [a] -> ByteArray
{-# INLINE unsafeByteArrayFromListN #-}
unsafeByteArrayFromListN n ys = runST $ do
    marr <- Primitive.newByteArray (n * Primitive.sizeOfType @a)
    let go !_ [] = return ()
        go !ix (x : xs) = do
            Primitive.writeByteArray marr ix x
            go (ix + 1) xs
    go 0 ys
    Primitive.unsafeFreezeByteArray marr
