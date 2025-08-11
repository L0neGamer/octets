{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RequiredTypeArguments #-}

module Text.Octet.Type where

import Data.Primitive.ByteArray
import GHC.Types
import GHC.Exts
import Control.DeepSeq
import Control.Monad.ST
-- import Data.Primitive
-- import Control.Monad.Primitive
import Data.List.NonEmpty qualified as NE
import Data.Word
-- import Control.Monad
-- import Data.Traversable
import Data.Foldable
import Data.Bifunctor

type role Octet nominal
newtype Octet encoding = MkOctet { unOctet :: ByteArray }
  deriving newtype (Semigroup, Monoid, Eq, NFData)

instance Ord (Octet encoding) where
  compare (MkOctet ba1) (MkOctet ba2)
    | sameByteArray ba1 ba2 = EQ
    | otherwise = compareByteArraysFrom ba1 0 ba2 0 minSize <> compare len1 len2
    where
    len1 = sizeofByteArray ba1
    len2 = sizeofByteArray ba2
    minSize = min len1 len2

type role OctetSlice nominal
data OctetSlice encoding = MkOctetSlice
  { octet :: {-# UNPACK #-} !(Octet encoding)
  , offset :: {-# UNPACK #-} !Int -- in bytes
  , size :: {-# UNPACK #-} !Int -- in bytes
  }

instance Eq (OctetSlice encoding) where
  (==) os1 os2 = size os1 == size os2 && EQ ==
    compareByteArraysFrom
      (unOctet (octet os1)) (offset os1)
      (unOctet (octet os2)) (offset os2)
      (size os1)

instance Ord (OctetSlice encoding) where
  compare os1 os2 =
    compareByteArraysFrom
      (unOctet (octet os1)) (offset os1)
      (unOctet (octet os2)) (offset os2)
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
      copyByteArray mba 0 (unOctet (octet os1)) (offset os1) size1
      copyByteArray mba size1 (unOctet (octet os2)) (offset os2) size2
      unsafeFreezeByteArray mba
    where
    size1 = size os1
    size2 = size os2

-- | Invariant: 0 <= startindex <= startindex + length for both bytearrays.
compareByteArraysFrom :: ByteArray -> Int -> ByteArray -> Int -> Int -> Ordering
compareByteArraysFrom (ByteArray ba1#) (I# ind1#) (ByteArray ba2#) (I# ind2#) (I# len#)
  = compare (I# (compareByteArrays# ba1# ind1# ba2# ind2# len#)) 0

sameByteArray :: ByteArray -> ByteArray -> Bool
sameByteArray (ByteArray ba1#) (ByteArray ba2#) = isTrue# (sameByteArray# ba1# ba2#)

-- doesn't do bounds or overlap checking
unsafeCopyByteArray
  :: PrimMonad m =>
     MutableByteArray (PrimState m) -- ^ destination array
  -> Int                -- ^ offset into destination array
  -> ByteArray          -- ^ source array
  -> Int                -- ^ offset into source array
  -> Int                -- ^ number of bytes to copy
  -> m ()
{-# INLINE unsafeCopyByteArray #-}
unsafeCopyByteArray (MutableByteArray dst) (I# doff) (ByteArray src) (I# soff) (I# sz) =
  primitive $ \s -> (# copyByteArray# src soff dst doff sz s, () #)

class OctetLike o where
  -- | Unsafe coercion
  coerceOctet :: o enc1 -> o enc2

  -- | Unsafe building
  fromByteArray :: ByteArray -> o enc

  empty :: o enc

  -- | Internal use, get the byte array, offset and length of the octet.
  -- For Octet, that's just the byte array, 0, and the length.
  deconstruct :: o enc -> (ByteArray, Int, Int)

  -- | Split an octet. A slice will slice, otherwise this is probably a copy.
  -- Splits before the index given; so `splitAt 0` gives (empty, octet).
  splitAt :: Int -> o enc -> (o enc, o enc)

  debugShow :: o enc -> String

instance OctetLike Octet where
  {-# INLINE coerceOctet #-}
  coerceOctet = MkOctet . unOctet

  {-# INLINE fromByteArray #-}
  fromByteArray = coerce

  {-# NOINLINE empty #-}
  empty = mempty

  {-# INLINE deconstruct #-}
  deconstruct (MkOctet ba) = (ba, 0, sizeofByteArray ba)

  {-# INLINE splitAt #-}
  splitAt takeSize o@(MkOctet ba)
    | takeSize <= 0 = (empty, o)
    | takeSize > sizeO = (o, empty)
    | otherwise = bimap fromByteArray fromByteArray $ runST $ do
        mba1 <- newByteArray takeSize
        mba2 <- newByteArray restSize
        unsafeCopyByteArray mba1 0 ba 0 takeSize
        unsafeCopyByteArray mba2 0 ba takeSize restSize
        liftA2 (,) (unsafeFreezeByteArray mba1) (unsafeFreezeByteArray mba2)
    where
    sizeO = sizeofByteArray ba
    restSize = sizeO - takeSize

  debugShow (MkOctet o) = "Octet " <> show o

instance OctetLike OctetSlice where
  {-# INLINE coerceOctet #-}
  coerceOctet MkOctetSlice {..} = MkOctetSlice {octet = coerceOctet octet,..}

  {-# INLINE fromByteArray #-}
  fromByteArray ba = MkOctetSlice (MkOctet ba) 0 (sizeofByteArray ba)

  {-# NOINLINE empty #-}
  empty = MkOctetSlice empty 0 0

  {-# INLINE deconstruct #-}
  deconstruct (MkOctetSlice {..}) = (unOctet octet, offset, size)

  {-# INLINE splitAt #-}
  splitAt takeSize o@(MkOctetSlice {..})
    | takeSize <= 0 = (empty, o)
    | takeSize >= sizeO = (o, empty)
    | otherwise = (MkOctetSlice {octet, offset, size = takeSize}, MkOctetSlice {octet, offset = offset + takeSize, size = restSize})
    where
    sizeO = size
    restSize = sizeO - takeSize

  debugShow (MkOctetSlice {..}) = "OctetSlice (" <> debugShow octet <> ") " <> show offset <> " " <> show size

-- | If the given octet is empty, we return the unpinned empty value, since
-- there's no point in actually pinning that.
toPinned :: OctetLike o => o enc -> o enc
toPinned o = runST $ do
  if size == 0
    then pure empty
    else do
      mba <- newPinnedByteArray size
      unsafeCopyByteArray mba 0 ba offset size
      fromByteArray <$> unsafeFreezeByteArray mba
  where
  (ba, offset, size) = deconstruct o

-- | If the given octet is empty, we return the unpinneed empty value.
toUnpinned :: OctetLike o => o enc -> o enc
toUnpinned o = runST $ do
  if size == 0
    then pure empty
    else do
      mba <- newByteArray size
      unsafeCopyByteArray mba 0 ba offset size
      fromByteArray <$> unsafeFreezeByteArray mba
  where
  (ba, offset, size) = deconstruct o

-- | Uses unpinned bytearray
fromListWith :: forall o a enc. OctetLike o => (a -> Maybe (NE.NonEmpty Word8)) -> [a] -> o enc
fromListWith _ [] = empty
fromListWith f as = fromByteArray $ runST $ do
  let destlen = 64
  mba <- newByteArray destlen
  go mba destlen 0 as
  where
  go :: MutableByteArray s -> Int -> Int -> [a] -> ST s ByteArray
  go mba destlen offset = \case
    [] -> do
      shrinkMutableByteArray mba offset
      unsafeFreezeByteArray mba
    x : xs
      | Just wordsNE <- f x -> do
          let len = length wordsNE
              newLen = len + offset
              newDestLen = 2 * destlen
          -- when there's not enough space
          (mba', destlen') <- if newLen >= destlen
            then (, newDestLen) <$> resizeMutableByteArray mba newDestLen
            else pure (mba, destlen)
          for_ (zip [0..] (NE.toList wordsNE)) $ \(ind, word) ->
            writeByteArray mba' (ind + offset) word
          go mba' destlen' newLen xs
      | otherwise -> go mba destlen offset xs

-- | Zero cost conversion to a slice.
toOctetSlice :: OctetLike o => o enc -> OctetSlice enc
toOctetSlice o = MkOctetSlice {octet = MkOctet ba,..}
  where
  (ba, offset, size) = deconstruct o

-- | Likely copy a bytearray into the octet. Won't copy if the offset is 0 and
-- the size matches the bytearray's size.
toOctet :: OctetLike o => o enc -> Octet enc
toOctet o
    | offset == 0 && size == sizeofByteArray ba = MkOctet ba
    | otherwise = MkOctet $ runST $ do
      mba <- newByteArray size
      unsafeCopyByteArray mba 0 ba offset size
      unsafeFreezeByteArray mba
  where
  (ba, offset, size) = deconstruct o

