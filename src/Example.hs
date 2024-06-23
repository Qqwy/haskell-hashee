{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -ddump-asm -O2 #-}
module Example where

import Hashee qualified
import Hashee.SipHash (SipHash(..))
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as ShortByteString
import Data.Word

example = do
  input <- (readLn :: IO (Word64, Word64)) -- pure ("hello world" :: ShortByteString)
  let alg = SipHash 0 0 :: SipHash 2 4
  let result = Hashee.hash alg input
  print result
