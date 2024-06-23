{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Hashee.SipHash where

-- Code is adapted from VincentHZ's hs-memory library

import           Data.Memory.Endian
import           Data.Word
import           Data.Bits
import           Data.Typeable (Typeable, Proxy(Proxy))
import           Control.Monad
import           Foreign.Ptr
import           Foreign.Storable
import Data.Primitive.ByteArray qualified as ByteArray
import GHC.TypeLits

import Hashee.HashingAlgorithm (HashingAlgorithm(..))

type SipHash14 = SipHash 1 3
type SipHash24 = SipHash 2 4
type SipHash48 = SipHash 4 8

-- | The SipHash algorithm
-- 'c' is the amount of compression rounds
-- 'd' is the amount of finalization rounds.
--
-- SipHash 2 4 is the most commonly used version.
data SipHash (c :: Nat) (d :: Nat) = SipHash {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64

instance (KnownNat c, KnownNat d) => HashingAlgorithm (SipHash c d) where
  data HasherState (SipHash c d) = SipHashState {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  type Digest (SipHash c d) = Word64

  runAlg key hasher =
    let 
       state = initSip key
       finalState = hasher state
    in finish finalState

  updateWord64 w = \state -> process @c @d state w
  updateWord32 w = updateWord64 (fromIntegral w)
  updateWord16 w = updateWord64 (fromIntegral w)
  updateWord8 w  = updateWord64 (fromIntegral w)

  -- SipHash already ensures bytestring length is taken into account
  updateByteString = updateBytes

  updateBytes ba = \state ->
    go 0 (len `div` 8) state
    where
      len = ByteArray.sizeofByteArray ba
      indexByte index = to64 (ByteArray.indexByteArray ba index)
      go index len state 
        | index == len = handleLeftover state
        | otherwise    =
          let state' = process @c @d state (ByteArray.indexByteArray ba index)
          in
          go (succ index) len state'
      handleLeftover state =
        let lenHighByte = (fromIntegral len `mod` 256) `unsafeShiftL` 56
        in
          process state $ case len `mod` 8 of
            0 -> 
                lenHighByte
            1 -> 
                lenHighByte 
                .|. indexByte (len - 1)
            2 -> 
              lenHighByte 
              .|. indexByte (len - 2)
              .|. indexByte (len - 1) `unsafeShiftL` 8
            3 -> 
              lenHighByte 
              .|. indexByte (len - 3)
              .|. indexByte (len - 2) `unsafeShiftL` 8
              .|. indexByte (len - 1) `unsafeShiftL` 16
            4 -> 
              lenHighByte 
              .|. indexByte (len - 4)
              .|. indexByte (len - 3) `unsafeShiftL` 8
              .|. indexByte (len - 2) `unsafeShiftL` 16
              .|. indexByte (len - 1) `unsafeShiftL` 24
            5 -> 
              lenHighByte 
              .|. indexByte (len - 5)
              .|. indexByte (len - 4) `unsafeShiftL` 8
              .|. indexByte (len - 3) `unsafeShiftL` 16
              .|. indexByte (len - 2) `unsafeShiftL` 24
              .|. indexByte (len - 1) `unsafeShiftL` 32
            6 -> 
              lenHighByte 
              .|. indexByte (len - 6)
              .|. indexByte (len - 5) `unsafeShiftL` 8
              .|. indexByte (len - 4) `unsafeShiftL` 16
              .|. indexByte (len - 3) `unsafeShiftL` 24
              .|. indexByte (len - 2) `unsafeShiftL` 32
              .|. indexByte (len - 1) `unsafeShiftL` 40
            7 -> 
              lenHighByte 
              .|. indexByte (len - 7)
              .|. indexByte (len - 6) `unsafeShiftL` 8
              .|. indexByte (len - 5) `unsafeShiftL` 16
              .|. indexByte (len - 4) `unsafeShiftL` 24
              .|. indexByte (len - 3) `unsafeShiftL` 32
              .|. indexByte (len - 2) `unsafeShiftL` 40
              .|. indexByte (len - 1) `unsafeShiftL` 48
            _ -> error "Unreachable"

-- -- | produce a siphash with a key and a memory pointer + length.
-- hash :: SipHash -> Ptr Word8 -> Int -> IO SipHash
-- hash = hashWith 2 4

-- -- | same as 'hash', except also specifies the number of sipround iterations for compression and digest.
-- hashWith :: Int       -- ^ siphash C
--          -> Int       -- ^ siphash D
--          -> SipHash    -- ^ key for the hash
--          -> Ptr Word8 -- ^ memory pointer
--          -> Int       -- ^ length of the data
--          -> IO SipHash
-- hashWith c d key startPtr totalLen = runHash (initSip key) startPtr totalLen
--   where runHash !st !ptr l
--             | l > 7     = peek (castPtr ptr) >>= \v -> runHash (process st (fromLE v)) (ptr `plusPtr` 8) (l-8)
--             | otherwise = do
--                 let !lengthBlock = (fromIntegral totalLen `mod` 256) `unsafeShiftL` 56
--                 (finish . process st) `fmap` case l of
--                     0 -> do return lengthBlock
--                     1 -> do v0 <- peekByteOff ptr 0
--                             return (lengthBlock .|. to64 v0)
--                     2 -> do (v0,v1) <- liftM2 (,) (peekByteOff ptr 0) (peekByteOff ptr 1)
--                             return (lengthBlock
--                                     .|. (to64 v1 `unsafeShiftL` 8)
--                                     .|. to64 v0)
--                     3 -> do (v0,v1,v2) <- liftM3 (,,) (peekByteOff ptr 0) (peekByteOff ptr 1) (peekByteOff ptr 2)
--                             return (    lengthBlock
--                                     .|. (to64 v2 `unsafeShiftL` 16)
--                                     .|. (to64 v1 `unsafeShiftL` 8)
--                                     .|. to64 v0)
--                     4 -> do (v0,v1,v2,v3) <- liftM4 (,,,) (peekByteOff ptr 0) (peekByteOff ptr 1) (peekByteOff ptr 2)
--                                                           (peekByteOff ptr 3)
--                             return (    lengthBlock
--                                     .|. (to64 v3 `unsafeShiftL` 24)
--                                     .|. (to64 v2 `unsafeShiftL` 16)
--                                     .|. (to64 v1 `unsafeShiftL` 8)
--                                     .|. to64 v0)
--                     5 -> do (v0,v1,v2,v3,v4) <- liftM5 (,,,,) (peekByteOff ptr 0) (peekByteOff ptr 1) (peekByteOff ptr 2)
--                                                               (peekByteOff ptr 3) (peekByteOff ptr 4)
--                             return (    lengthBlock
--                                     .|. (to64 v4 `unsafeShiftL` 32)
--                                     .|. (to64 v3 `unsafeShiftL` 24)
--                                     .|. (to64 v2 `unsafeShiftL` 16)
--                                     .|. (to64 v1 `unsafeShiftL` 8)
--                                     .|. to64 v0)
--                     6 -> do v0 <- peekByteOff ptr 0
--                             v1 <- peekByteOff ptr 1
--                             v2 <- peekByteOff ptr 2
--                             v3 <- peekByteOff ptr 3
--                             v4 <- peekByteOff ptr 4
--                             v5 <- peekByteOff ptr 5
--                             return (    lengthBlock
--                                     .|. (to64 v5 `unsafeShiftL` 40)
--                                     .|. (to64 v4 `unsafeShiftL` 32)
--                                     .|. (to64 v3 `unsafeShiftL` 24)
--                                     .|. (to64 v2 `unsafeShiftL` 16)
--                                     .|. (to64 v1 `unsafeShiftL` 8)
--                                     .|. to64 v0)
--                     7 -> do v0 <- peekByteOff ptr 0
--                             v1 <- peekByteOff ptr 1
--                             v2 <- peekByteOff ptr 2
--                             v3 <- peekByteOff ptr 3
--                             v4 <- peekByteOff ptr 4
--                             v5 <- peekByteOff ptr 5
--                             v6 <- peekByteOff ptr 6
--                             return (    lengthBlock
--                                     .|. (to64 v6 `unsafeShiftL` 48)
--                                     .|. (to64 v5 `unsafeShiftL` 40)
--                                     .|. (to64 v4 `unsafeShiftL` 32)
--                                     .|. (to64 v3 `unsafeShiftL` 24)
--                                     .|. (to64 v2 `unsafeShiftL` 16)
--                                     .|. (to64 v1 `unsafeShiftL` 8)
--                                     .|. to64 v0)
--                     _ -> error "siphash: internal error: cannot happens"

{-# INLINE to64 #-}
to64 :: Word8 -> Word64
to64 = fromIntegral

{-# INLINE process #-}
process :: forall c d. (KnownNat c, KnownNat d) => (HasherState (SipHash c d)) -> Word64 -> (HasherState (SipHash c d))
process istate m = newState
    where newState = postInject $! runRoundsCompression @c @d $! preInject istate
          preInject  (SipHashState v0 v1 v2 v3) = SipHashState v0 v1 v2 (v3 `xor` m)
          postInject (SipHashState v0 v1 v2 v3) = SipHashState (v0 `xor` m) v1 v2 v3

{-# INLINE finish #-}
finish :: forall c d. (KnownNat c, KnownNat d) => (HasherState (SipHash c d)) -> Digest (SipHash c d)
finish istate = getDigest $! runRoundsDigest @c @d $! preInject istate
    where getDigest (SipHashState v0 v1 v2 v3) = (v0 `xor` v1 `xor` v2 `xor` v3)
          preInject (SipHashState v0 v1 v2 v3) = SipHashState v0 v1 (v2 `xor` 0xff) v3

{-# INLINE doRound #-}
doRound :: (HasherState (SipHash c d)) -> HasherState (SipHash c d)
doRound (SipHashState v0 v1 v2 v3) =
        let !v0'    = v0 + v1
            !v2'    = v2 + v3
            !v1'    = v1 `rotateL` 13
            !v3'    = v3 `rotateL` 16
            !v1''   = v1' `xor` v0'
            !v3''   = v3' `xor` v2'
            !v0''   = v0' `rotateL` 32
            !v2''   = v2' + v1''
            !v0'''  = v0'' + v3''
            !v1'''  = v1'' `rotateL` 17
            !v3'''  = v3'' `rotateL` 21
            !v1'''' = v1''' `xor` v2''
            !v3'''' = v3''' `xor` v0'''
            !v2'''  = v2'' `rotateL` 32
        in SipHashState v0''' v1'''' v2''' v3''''

{-# INLINE runRoundsCompression #-}
runRoundsCompression :: forall c d. (KnownNat c, KnownNat d) => (HasherState (SipHash c d)) -> HasherState (SipHash c d)
runRoundsCompression st
    | (natWord @d) == 2    = doRound $! doRound st
    | otherwise = loopRounds (natWord @c) st

{-# INLINE runRoundsDigest #-}
runRoundsDigest :: forall c d. (KnownNat c, KnownNat d) => HasherState (SipHash c d) -> HasherState (SipHash c d)
runRoundsDigest st
    | (natWord @d) == 4    = doRound $! doRound $! doRound $! doRound st
    | otherwise = loopRounds (natWord @c) st

{-# INLINE loopRounds #-}
loopRounds :: Word64 -> HasherState (SipHash c d) -> HasherState (SipHash c d)
loopRounds 1 !v = doRound v
loopRounds n !v = loopRounds (n-1) (doRound v)

{-# INLINE initSip #-}
initSip :: forall c d. SipHash c d -> HasherState (SipHash c d)
initSip (SipHash k0 k1) = SipHashState (k0 `xor` 0x736f6d6570736575)
                                        (k1 `xor` 0x646f72616e646f6d)
                                        (k0 `xor` 0x6c7967656e657261)
                                        (k1 `xor` 0x7465646279746573)

natWord :: forall n. KnownNat n => Word64
natWord = fromIntegral $ natVal (Proxy @n)
