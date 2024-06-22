module Hashee (Hashee(hash)) where

import Hashee.Hasher (Hasher(Hasher))
import Hashee.Hasher qualified as Hasher
import Data.Primitive.ByteArray (ByteArray)
import Data.Primitive.ByteArray qualified as ByteArray
import Data.Word
import Data.Int
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as ShortByteString
import Data.Void
import Data.Tuple (Solo(..))

class Hashee a where
  hash :: a -> Hasher

instance Hashee Word64 where
  hash !x = Hasher.word64 x

instance Hashee Word32 where
  hash !x = Hasher.word32 x

instance Hashee Int64 where
  hash !x = Hasher.int64 x

instance Hashee Int32 where
  hash !x = Hasher.int32 x

instance Hashee ByteArray where
  hash !ba = Hasher.int (ByteArray.sizeofByteArray ba) <> Hasher.bytes ba

instance Hashee ShortByteString where
  hash !bs = Hasher.int (ShortByteString.length bs) <> Hasher.bytes (ShortByteString.unShortByteString bs)

instance Hashee Void where
  hash v = case v of {}

instance Hashee () where
  hash () = mempty

instance (Hashee a) => Hashee (Solo a) where
  hash (MkSolo a) = hash a

instance (Hashee a, Hashee b) => Hashee (a, b) where
  hash (a, b) = hash a <> hash b

instance (Hashee a, Hashee b, Hashee c) => Hashee (a, b, c) where
  hash (a, b, c) = hash a <> hash b <> hash c

instance (Hashee a) => Hashee (Maybe a) where
    hash Nothing = Hasher.int8 0
    hash (Just b) = Hasher.int8 1 <> hash b

instance (Hashee a, Hashee b) => Hashee (Either a b) where
    hash (Left a) = Hasher.int8 0 <> hash a
    hash (Right b) = Hasher.int8 1 <> hash b
