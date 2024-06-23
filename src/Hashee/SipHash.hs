{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
module Hashee.SipHash where

-- Code is adapted from VincentHZ's hs-memory library

import           Data.Memory.Endian
import           Data.Word
import           Data.Bits
import           Data.Typeable (Typeable, Proxy(Proxy))
import           Control.Monad
import           Foreign.Ptr
import           Foreign.Storable
import Hashee.Internal.Iterate (Iterate)
import Hashee.Internal.Iterate qualified as Iterate
import Data.Primitive.ByteArray qualified as ByteArray
import GHC.TypeLits

import Hashee.HashingAlgorithm (HashingAlgorithm(..))

type SipHash13 = SipHash 1 3
type SipHash24 = SipHash 2 4
type SipHash48 = SipHash 4 8

-- | The SipHash algorithm
-- 'c' is the amount of compression rounds
-- 'd' is the amount of finalization rounds.
--
-- SipHash 2 4 is the most commonly used version.
data SipHash (c :: Nat) (d :: Nat) = SipHash {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64

instance (Iterate c, Iterate d) => HashingAlgorithm (SipHash c d) where
  data HasherState (SipHash c d) = SipHashState {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  type Digest (SipHash c d) = Word64

  {-# INLINE runAlg #-}
  runAlg key hasher =
    let 
       state = initialize key
       finalState = hasher state
    in finish finalState

  {-# INLINE updateWord64 #-}
  updateWord64 w = \state -> process @c @d state w
  {-# INLINE updateWord32 #-}
  updateWord32 w = updateWord64 (fromIntegral w)
  {-# INLINE updateWord16 #-}
  updateWord16 w = updateWord64 (fromIntegral w)
  {-# INLINE updateWord8 #-}
  updateWord8 w  = updateWord64 (fromIntegral w)

  -- SipHash already ensures bytestring length is taken into account
  {-# INLINE updateByteString #-}
  updateByteString = updateBytes

  {-# INLINE updateBytes #-}
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

{-# INLINE to64 #-}
to64 :: Word8 -> Word64
to64 = fromIntegral

{-# INLINE process #-}
process :: forall c d. (Iterate c, Iterate d) => (HasherState (SipHash c d)) -> Word64 -> (HasherState (SipHash c d))
process istate m = newState
    where newState = postInject $! runRoundsCompression @c @d $! preInject istate
          preInject  (SipHashState v0 v1 v2 v3) = SipHashState v0 v1 v2 (v3 `xor` m)
          postInject (SipHashState v0 v1 v2 v3) = SipHashState (v0 `xor` m) v1 v2 v3

{-# INLINE finish #-}
finish :: forall c d. (Iterate c, Iterate d) => (HasherState (SipHash c d)) -> Digest (SipHash c d)
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
runRoundsCompression :: forall c d. (Iterate c) => (HasherState (SipHash c d)) -> HasherState (SipHash c d)
runRoundsCompression st = loopRounds @c st

{-# INLINE runRoundsDigest #-}
runRoundsDigest :: forall c d. (Iterate d) => HasherState (SipHash c d) -> HasherState (SipHash c d)
runRoundsDigest st = loopRounds @d st

loopRounds :: forall i c d. (Iterate i) => HasherState (SipHash c d) -> HasherState (SipHash c d)
loopRounds !v = Iterate.iter @i doRound v

{-# INLINE initialize #-}
initialize :: forall c d. SipHash c d -> HasherState (SipHash c d)
initialize (SipHash k0 k1) = SipHashState (k0 `xor` 0x736f6d6570736575)
                                        (k1 `xor` 0x646f72616e646f6d)
                                        (k0 `xor` 0x6c7967656e657261)
                                        (k1 `xor` 0x7465646279746573)

natToWord :: forall n. KnownNat n => Word64
natToWord = fromIntegral $ natVal (Proxy @n)
