{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
module Hashee.HasherState where

import Data.Primitive (Prim)
import Data.Primitive qualified as Primitive
import Data.Primitive.ByteArray (ByteArray(ByteArray))
import Data.Primitive.ByteArray qualified as ByteArray
import Data.Word
import Data.Int
import Control.Monad.ST (runST)

-- | Knows how to construct a new Hasher
--
-- `Hasher`s are stateful, `HasherBuilder`s are not,
-- and are the way you can 'reset' the state of a `Hasher`.
class HasherState hr => HasherBuilder hb hr | hb -> hr where
    build :: hb -> hr

class HasherState hr where
  -- | The final output of running the hashing algorithm to completion.
  -- Usually a `Word64`, 
  -- sometimes a `Word32`
  -- or very rarely a `Word128` (i.e. `(Word64, Word64)`)
  --
  -- Note that it's okay to keep calling `update*` on the hasher 
  -- even after calling `digest` on it.
  type Digest hr

  digest :: hr -> Digest hr

  updateBytes :: ByteArray -> hr -> hr

  updateWord64 :: Word64 -> hr -> hr
  updateWord64 !x = updateBytes $ unsafeByteArrayFromListN 1 [x]

  updateWord32 :: Word32 -> hr -> hr
  updateWord32 !x = updateBytes $ unsafeByteArrayFromListN 1 [x]

  updateWord16 :: Word16 -> hr -> hr
  updateWord16 !x = updateBytes $ unsafeByteArrayFromListN 1 [x]

  updateWord8 :: Word8 -> hr -> hr
  updateWord8 !x = updateBytes $ unsafeByteArrayFromListN 1 [x]

  updateInt64 :: Int64 -> hr -> hr
  updateInt64 !x = updateWord64 (fromIntegral x)

  updateInt32 :: Int32 -> hr -> hr
  updateInt32 !x = updateWord32 (fromIntegral x)

  updateInt16 :: Int16 -> hr -> hr
  updateInt16 !x = updateWord16 (fromIntegral x)

  updateInt8 :: Int8 -> hr -> hr
  updateInt8 !x = updateWord8 (fromIntegral x)

  -- TODO maybe special updateLen?



-- | Create a 'ByteArray' from a list of a known length. If the length
-- of the list does not match the given length, this throws an exception.
unsafeByteArrayFromListN :: forall a. Prim a => Int -> [a] -> ByteArray
unsafeByteArrayFromListN n ys = runST $ do
    marr <- Primitive.newByteArray (n * Primitive.sizeOfType @a)
    let go !_ [] = return ()
        go !ix (x : xs) = do
            Primitive.writeByteArray marr ix x
            go (ix + 1) xs
    go 0 ys
    Primitive.unsafeFreezeByteArray marr

