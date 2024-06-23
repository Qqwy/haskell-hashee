{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -ddump-asm -O2 #-}
module Hashee (Hashee(buildHasher), hash, example) where

import Hashee.Hasher (Hasher)
import Hashee.Hasher qualified as Hasher
import Hashee.HashingAlgorithm (HashingAlgorithm(Digest))
import Data.Primitive.ByteArray (ByteArray)
import Data.Primitive.ByteArray qualified as ByteArray
import Data.Word
import Data.Int
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as ShortByteString
import Data.Void
import Data.Tuple (Solo(..))
import Hashee.SipHash

example = do
  input <- pure ("hello world" :: ShortByteString)
  let alg = SipHash 0 0 :: SipHash48
  let result = hash alg input
  print result

hash :: (Hashee a, HashingAlgorithm h) => h -> a -> Digest h
{-# INLINE hash #-}
hash !alg !val = Hasher.runHasher alg (buildHasher val)

class Hashee a where
  buildHasher :: a -> Hasher

instance Hashee Word64 where
  {-# INLINE buildHasher #-}
  buildHasher !x = Hasher.word64 x

instance Hashee Word where
  {-# INLINE buildHasher #-}
  buildHasher !x = Hasher.word x

instance Hashee Int where
  {-# INLINE buildHasher #-}
  buildHasher !x = Hasher.int x

instance Hashee Word32 where
  {-# INLINE buildHasher #-}
  buildHasher !x = Hasher.word32 x

instance Hashee Int64 where
  {-# INLINE buildHasher #-}
  buildHasher !x = Hasher.int64 x

instance Hashee Int32 where
  {-# INLINE buildHasher #-}
  buildHasher !x = Hasher.int32 x

instance Hashee ByteArray where
  {-# INLINE buildHasher #-}
  buildHasher !ba = Hasher.bytes ba

instance Hashee ShortByteString where
  {-# INLINE buildHasher #-}
  buildHasher !bs = Hasher.bytes (ShortByteString.unShortByteString bs)

instance Hashee Void where
  {-# INLINE buildHasher #-}
  buildHasher v = case v of {}

instance Hashee () where
  {-# INLINE buildHasher #-}
  buildHasher () = mempty

instance (Hashee a) => Hashee (Solo a) where
  {-# INLINE buildHasher #-}
  buildHasher (MkSolo a) = buildHasher a

instance (Hashee a, Hashee b) => Hashee (a, b) where
  {-# INLINE buildHasher #-}
  buildHasher (a, b) = buildHasher a <> buildHasher b

instance (Hashee a, Hashee b, Hashee c) => Hashee (a, b, c) where
  {-# INLINE buildHasher #-}
  buildHasher (a, b, c) = buildHasher a <> buildHasher b <> buildHasher c

instance (Hashee a) => Hashee (Maybe a) where
  {-# INLINE buildHasher #-}
  buildHasher Nothing = Hasher.int8 0
  buildHasher (Just b) = Hasher.int8 1 <> buildHasher b

instance (Hashee a, Hashee b) => Hashee (Either a b) where
  {-# INLINE buildHasher #-}
  buildHasher (Left a) = Hasher.int8 0 <> buildHasher a
  buildHasher (Right b) = Hasher.int8 1 <> buildHasher b
