module Hashee (Hashee(buildHasher), hash) where

import Hashee.Hasher (Hasher(Hasher))
import Hashee.Hasher qualified as Hasher
import Hashee.HashingAlgorithm (HashingAlgorithm(Digest))
import Hashee.Hasher qualified as Hasher
import Data.Primitive.ByteArray (ByteArray)
import Data.Primitive.ByteArray qualified as ByteArray
import Data.Word
import Data.Int
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as ShortByteString
import Data.Void
import Data.Tuple (Solo(..))

hash :: (Hashee a, HashingAlgorithm h) => h -> a -> Digest h
hash alg val = Hasher.runHasher alg (buildHasher val)

class Hashee a where
  buildHasher :: a -> Hasher

instance Hashee Word64 where
  buildHasher !x = Hasher.word64 x

instance Hashee Word32 where
  buildHasher !x = Hasher.word32 x

instance Hashee Int64 where
  buildHasher !x = Hasher.int64 x

instance Hashee Int32 where
  buildHasher !x = Hasher.int32 x

instance Hashee ByteArray where
  buildHasher !ba = Hasher.int (ByteArray.sizeofByteArray ba) <> Hasher.bytes ba

instance Hashee ShortByteString where
  buildHasher !bs = Hasher.int (ShortByteString.length bs) <> Hasher.bytes (ShortByteString.unShortByteString bs)

instance Hashee Void where
  buildHasher v = case v of {}

instance Hashee () where
  buildHasher () = mempty

instance (Hashee a) => Hashee (Solo a) where
  buildHasher (MkSolo a) = buildHasher a

instance (Hashee a, Hashee b) => Hashee (a, b) where
  buildHasher (a, b) = buildHasher a <> buildHasher b

instance (Hashee a, Hashee b, Hashee c) => Hashee (a, b, c) where
  buildHasher (a, b, c) = buildHasher a <> buildHasher b <> buildHasher c

instance (Hashee a) => Hashee (Maybe a) where
    buildHasher Nothing = Hasher.int8 0
    buildHasher (Just b) = Hasher.int8 1 <> buildHasher b

instance (Hashee a, Hashee b) => Hashee (Either a b) where
    buildHasher (Left a) = Hasher.int8 0 <> buildHasher a
    buildHasher (Right b) = Hasher.int8 1 <> buildHasher b
