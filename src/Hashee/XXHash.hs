{-# LANGUAGE TypeFamilies #-}
module Hashee.XXHash where

import Hashee.HasherState
import Data.Primitive.ByteArray qualified as ByteArray
import Data.Digest.XXHash.FFI.C (XXH3State)
import Data.Digest.XXHash.FFI.C qualified as XXHash
import Data.Word (Word64)
import GHC.IO (unsafePerformIO)

data XXH3 = XXH3 Word64
data LiftedXXH3State = LS !XXH3State

instance HashingAlgorithm XXH3 where
  newtype HasherState XXH3 = HasherState_XXH3 LiftedXXH3State
  type Digest XXH3 = Word64

  runAlg (XXH3 seed) hasher = unsafePerformIO $ XXHash.allocaXXH3State $ \state -> do
    let seedCULLong = fromIntegral seed
    XXHash.c_xxh3_64bits_reset_withSeed state seedCULLong
    let !(HasherState_XXH3 (LS finalState)) = hasher (HasherState_XXH3 (LS state))
    digest <- XXHash.c_xxh3_64bits_digest finalState
    pure (fromIntegral $ digest)

  updateBytes !ba (HasherState_XXH3 (LS state)) = unsafePerformIO $ ByteArray.withByteArrayContents ba $ \ptr -> do
    let len = fromIntegral $ ByteArray.sizeofByteArray ba
    XXHash.c_xxh3_64bits_update state ptr len
    pure (HasherState_XXH3 (LS state))
