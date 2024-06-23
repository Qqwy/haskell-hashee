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
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as ShortByteString
import Data.Text.Internal (Text(..))
import Data.Text.Lazy qualified as LazyText
import Data.Text.Short (ShortText)
import Data.Text.Short qualified as ShortText
import Foreign.C.String (CStringLen)
import Foreign.Storable (peekByteOff)

import Data.Word (Word8)

import GHC.IO (IO(IO))
import GHC.Exts (realWorld#)


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

instance ByteIndexable ShortByteString where
  {-# INLINE size #-}
  size = size . ShortByteString.unShortByteString
  {-# INLINE unsafeIndex #-}
  unsafeIndex =  unsafeIndex . ShortByteString.unShortByteString

instance ByteIndexable ShortText where
  {-# INLINE size #-}
  size = size . ShortText.toShortByteString
  {-# INLINE unsafeIndex #-}
  unsafeIndex =  unsafeIndex . ShortText.toShortByteString

instance ByteIndexable CStringLen where
  size (_ptr, len) = len
  unsafeIndex (ptr, _len) ix = accursedUnutterablePerformIO $ do 
    peekByteOff ptr ix

{-# INLINE accursedUnutterablePerformIO #-}
accursedUnutterablePerformIO :: IO a -> a
accursedUnutterablePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
