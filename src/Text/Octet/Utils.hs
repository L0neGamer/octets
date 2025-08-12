
module Text.Octet.Utils
  ( Text.Octet.Utils.indexByteArray
  , compareByteArraysFrom
  , sameByteArray
  , unsafeCopyByteArray
  ) where

import Data.Primitive.ByteArray
import GHC.Types
import GHC.Exts
import Control.Monad.ST
import Control.Monad.Primitive
import Data.Word
import GHC.Word

-- | Index into a bytearray to get a byte.
indexByteArray :: ByteArray -> Int -> Word8
indexByteArray (ByteArray ba#) (I# i#) = W8# $ indexWord8Array# ba# i#

-- | Invariant: 0 <= startindex <= startindex + length for both bytearrays.
compareByteArraysFrom :: ByteArray -> Int -> ByteArray -> Int -> Int -> Ordering
compareByteArraysFrom (ByteArray ba1#) (I# ind1#) (ByteArray ba2#) (I# ind2#) (I# len#)
  = compare (I# (compareByteArrays# ba1# ind1# ba2# ind2# len#)) 0

-- | Effectively pointer equality.
sameByteArray :: ByteArray -> ByteArray -> Bool
sameByteArray (ByteArray ba1#) (ByteArray ba2#) = isTrue# (sameByteArray# ba1# ba2#)

-- doesn't do bounds or overlap checking
unsafeCopyByteArray
  :: MutableByteArray s -- ^ destination array
  -> Int                -- ^ offset into destination array
  -> ByteArray          -- ^ source array
  -> Int                -- ^ offset into source array
  -> Int                -- ^ number of bytes to copy
  -> ST s ()
unsafeCopyByteArray (MutableByteArray dst) (I# doff) (ByteArray src) (I# soff) (I# sz) =
  primitive $ \s -> (# copyByteArray# src soff dst doff sz s, () #)
