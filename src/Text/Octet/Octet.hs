
module Text.Octet.Octet 
  ( Octet (..)
  , emptyOctet
  , toOctet

  , module Text.Octet.Class
  ) where

import Data.Primitive.ByteArray
import Control.DeepSeq
import Control.Monad.ST
import Data.Bifunctor
import Text.Octet.Class
import Text.Octet.Utils qualified as Utils

type role Octet nominal
newtype Octet encoding = MkOctet { unOctet :: ByteArray }
  deriving newtype (Semigroup, Eq, NFData)

instance Ord (Octet encoding) where
  compare (MkOctet ba1) (MkOctet ba2)
    | Utils.sameByteArray ba1 ba2 = EQ
    | otherwise = Utils.compareByteArraysFrom ba1 0 ba2 0 minSize <> compare len1 len2
    where
    len1 = sizeofByteArray ba1
    len2 = sizeofByteArray ba2
    minSize = min len1 len2

instance Monoid (Octet encoding) where
  mempty = emptyOctet

{-# NOINLINE emptyOctet #-}
emptyOctet :: Octet encoding
emptyOctet = MkOctet emptyByteArray

instance OctetLike Octet where
  {-# INLINABLE coerceOctet #-}
  coerceOctet = MkOctet . unOctet

  {-# INLINABLE fromByteArray #-}
  fromByteArray = MkOctet

  empty = mempty

  {-# INLINABLE deconstruct #-}
  deconstruct (MkOctet ba) = (ba, 0, sizeofByteArray ba)

  {-# INLINABLE splitAt #-}
  splitAt takeSize o@(MkOctet ba)
    | takeSize <= 0 = (empty, o)
    | takeSize > sizeO = (o, empty)
    | otherwise = bimap fromByteArray fromByteArray $ runST $ do
        mba1 <- newByteArray takeSize
        mba2 <- newByteArray restSize
        Utils.unsafeCopyByteArray mba1 0 ba 0 takeSize
        Utils.unsafeCopyByteArray mba2 0 ba takeSize restSize
        liftA2 (,) (unsafeFreezeByteArray mba1) (unsafeFreezeByteArray mba2)
    where
    sizeO = sizeofByteArray ba
    restSize = sizeO - takeSize

  debugShow (MkOctet o) = "Octet " <> show o

-- | Likely copy a bytearray into the octet. Won't copy if the offset is 0 and
-- the size matches the bytearray's size.
{-# INLINABLE toOctet #-}
toOctet :: OctetLike o => o enc -> Octet enc
toOctet o
    | offset == 0 && size == sizeofByteArray ba = MkOctet ba
    | otherwise = MkOctet $ runST $ do
      mba <- newByteArray size
      Utils.unsafeCopyByteArray mba 0 ba offset size
      unsafeFreezeByteArray mba
  where
  (ba, offset, size) = deconstruct o
