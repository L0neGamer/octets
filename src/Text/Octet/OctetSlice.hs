
module Text.Octet.OctetSlice
  ( OctetSlice (..)
  , emptySlice
  , toOctetSlice

  , module Text.Octet.Class
  ) where

import Data.Primitive.ByteArray
import Control.DeepSeq
import Control.Monad.ST
import Text.Octet.Class
import Text.Octet.Utils as Utils

type role OctetSlice nominal
data OctetSlice encoding = MkOctetSlice
  { octet :: {-# UNPACK #-} !ByteArray
  , offset :: {-# UNPACK #-} !Int -- in bytes
  , size :: {-# UNPACK #-} !Int -- in bytes
  }

instance Eq (OctetSlice encoding) where
  (==) os1 os2 = sameSlice os1 os2 || diffBAs
    where
    ba1 = octet os1
    ba2 = octet os2
    diffBAs = size os1 == size os2 && EQ ==
      compareByteArraysFrom
        ba1 (offset os1)
        ba2 (offset os2)
        (size os1)

instance Ord (OctetSlice encoding) where
  compare os1 os2
    | sameSlice os1 os2 = EQ
    | otherwise =
        compareByteArraysFrom
          (octet os1) (offset os1)
          (octet os2) (offset os2)
          minSize
        <> compare len1 len2
    where
    len1 = size os1
    len2 = size os2
    minSize = min len1 len2

instance NFData (OctetSlice encoding) where
  rnf !_ = ()

instance Semigroup (OctetSlice encoding) where
  (<>) os1 os2
    | size os1 == 0 = os2
    | size os2 == 0 = os1
    | otherwise = fromByteArray $ runST $ do
      mba <- newByteArray (size1 + size2)
      unsafeCopyByteArray mba 0 (octet os1) (offset os1) size1
      unsafeCopyByteArray mba size1 (octet os2) (offset os2) size2
      unsafeFreezeByteArray mba
    where
    size1 = size os1
    size2 = size os2

instance Monoid (OctetSlice encoding) where
  mempty = emptySlice

{-# NOINLINE emptySlice #-}
emptySlice :: OctetSlice encoding
emptySlice = MkOctetSlice emptyByteArray 0 0

sameSlice :: OctetSlice encoding -> OctetSlice encoding -> Bool
sameSlice os1 os2 = sameByteArray (octet os1) (octet os2) &&
  size os1 == size os2 &&
  offset os1 == offset os2

instance OctetLike OctetSlice where
  {-# INLINABLE coerceOctet #-}
  coerceOctet MkOctetSlice {..} = MkOctetSlice {..}

  {-# INLINABLE fromByteArray #-}
  fromByteArray ba = MkOctetSlice ba 0 (sizeofByteArray ba)

  empty = emptySlice

  {-# INLINABLE deconstruct #-}
  deconstruct (MkOctetSlice {..}) = (octet, offset, size)

  {-# INLINABLE splitAt #-}
  splitAt takeSize o@(MkOctetSlice {..})
    | takeSize <= 0 = (empty, o)
    | takeSize >= sizeO = (o, empty)
    | otherwise = (MkOctetSlice {octet, offset, size = takeSize}, MkOctetSlice {octet, offset = offset + takeSize, size = restSize})
    where
    sizeO = size
    restSize = sizeO - takeSize

  debugShow (MkOctetSlice {..}) = "OctetSlice (" <> show octet <> ") " <> show offset <> " " <> show size

-- | Zero cost conversion to a slice.
toOctetSlice :: OctetLike o => o enc -> OctetSlice enc
toOctetSlice o = MkOctetSlice {octet = ba,..}
  where
  (ba, offset, size) = deconstruct o
