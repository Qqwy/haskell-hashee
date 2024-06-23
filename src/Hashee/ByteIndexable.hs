{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Hashee.ByteIndexable where

import Data.Primitive.ByteArray (ByteArray)
import Data.Primitive.ByteArray qualified as ByteArray

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.ByteString.Unsafe qualified as ByteString.Unsafe

-- import Data.Text qualified as Text
import Data.Text.Internal (Text(..))

import Data.Word (Word8)

import Data.Text.Lazy qualified as LazyText

import GHC.IO (IO(IO))
import GHC.Exts (realWorld#)
import Foreign.C.String (CStringLen)
import Foreign.Storable (peekByteOff)

class ByteIndexable ba where
  size :: ba -> Int
  unsafeIndex :: ba -> Int -> Word8

instance ByteIndexable ByteArray where
  {-# INLINE size #-}
  size = ByteArray.sizeofByteArray
  {-# INLINE unsafeIndex #-}
  unsafeIndex = ByteArray.indexByteArray

instance ByteIndexable ByteString where
  {-# INLINE size #-}
  size bs = ByteString.length bs
  {-# INLINE unsafeIndex #-}
  unsafeIndex bs ix = ByteString.Unsafe.unsafeIndex bs ix

instance ByteIndexable LazyByteString where
  {-# INLINE size #-}
  size bs = fromIntegral $ LazyByteString.length bs
  {-# INLINE unsafeIndex #-}
  unsafeIndex bs ix = LazyByteString.index bs (fromIntegral ix)

instance ByteIndexable Text where
  {-# INLINE size #-}
  size (Text _arr _offset len) = len
  {-# INLINE unsafeIndex #-}
  unsafeIndex (Text arr offset _len) ix = unsafeIndex arr (offset + ix)

instance ByteIndexable LazyText.Text where
  {-# INLINE size #-}
  size text = LazyText.foldlChunks (\n chunk -> n + size chunk) 0 text
  {-# INLINE unsafeIndex #-}
  unsafeIndex text ix = go (LazyText.toChunks text) ix
    where
      go [] _ = error "Incorrect usage of ByteIndexable.index for LazyText"
      go (t : ts) ix'
        | ix' < size t = unsafeIndex t ix'
        | otherwise   = go ts (ix' - size t)

instance ByteIndexable CStringLen where
  size (_ptr, len) = len
  unsafeIndex (ptr, _len) ix = accursedUnutterablePerformIO $ do 
    peekByteOff ptr ix

{-# INLINE accursedUnutterablePerformIO #-}
accursedUnutterablePerformIO :: IO a -> a
accursedUnutterablePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
