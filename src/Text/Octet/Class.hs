
module Text.Octet.Class
  ( OctetLike (..)

  , foldrWith
  , foldlWith
  , toPinned
  , toUnpinned
  , fromListWith
  ) where

import Data.Primitive.ByteArray
import Control.Monad.ST
import Data.List.NonEmpty qualified as NE
import Data.Word
import Data.Foldable
import Data.Functor
import Control.Monad
import Text.Octet.Utils as Utils

class OctetLike o where
  -- | Unsafe coercion
  coerceOctet :: o enc1 -> o enc2

  -- | Unsafe building
  fromByteArray :: ByteArray -> o enc

  -- | The empty octet. Should ideally be marked as NOINLINE.
  empty :: o enc

  -- | Internal use, get the byte array, offset and length of the octet.
  -- For Octet, that's just the byte array, 0, and the length.
  deconstruct :: o enc -> (ByteArray, Int, Int)

  -- | Split an octet. A slice will slice, otherwise this is probably a copy.
  -- Splits before the index given; so `splitAt 0` gives (empty, octet).
  splitAt :: Int -> o enc -> (o enc, o enc)

  debugShow :: o enc -> String

-- | Fold over an octet when given:
--
-- - An amount of bytes to peek from the left - if there is not enough bytes
--    left, then all the bytes that can be gotten are given
-- - A function which, when given all the peeked bytes, always
--    returns an `a` value and how many were used (which must be >= 1).
-- - A folding function which takes the previous `a` value and a `b` value to
--    return another `b` value
-- - A zero `b` value
-- - The octet
foldrWith :: forall o enc a b. OctetLike o => Int -> (NE.NonEmpty Word8 -> (a , Int)) -> (a -> b -> b) -> b -> o enc -> b
foldrWith peek getNextOffset f z o = doFold offset
  where
  (ba, offset, size) = deconstruct o
  maxOffset = offset + size

  getNext :: Int -> Maybe (Int, a)
  getNext currentOffset = guard (currentOffset < maxOffset) $> (currentOffset + used, nextValue)
    where
    (nextValue, used) = getNextOffset bytes
    bytes = Utils.indexByteArray ba <$> currentOffset NE.:| [currentOffset + 1 .. min (currentOffset + peek) maxOffset]

  doFold :: Int -> b
  doFold currentOffset = case getNext currentOffset of
    Nothing -> z
    Just (nextOffset, a) -> f a (doFold nextOffset)

-- | Fold over an octet when given:
--
-- - An amount of bytes to peek from the right - if there is not enough bytes
--    left, then all the bytes that can be gotten are given
-- - A function which, when given all the peeked bytes, always
--    returns an `a` value and how many were used (which must be >= 1). The bytes
--    are given right to left
-- - A folding function which takes the previous `a` value and a `b` value to
--    return another `b` value
-- - A zero `b` value
-- - The octet
foldlWith :: forall o enc a b. OctetLike o => Int -> (NE.NonEmpty Word8 -> (a , Int)) -> (b -> a -> b) -> b -> o enc -> b
foldlWith peek getNextOffset f z o = doFold startOffset
  where
  (ba, offset, size) = deconstruct o
  maxOffset = offset + size
  startOffset = maxOffset - 1

  getNext :: Int -> Maybe (Int, a)
  getNext currentOffset = guard (currentOffset >= offset) $> (currentOffset - used, nextValue)
    where
    (nextValue, used) = getNextOffset bytes
    bytes = Utils.indexByteArray ba <$> currentOffset NE.:| [currentOffset - 1, currentOffset - 2 .. max (currentOffset - peek) offset]

  doFold :: Int -> b
  doFold currentOffset = case getNext currentOffset of
    Nothing -> z
    Just (nextOffset, a) -> f (doFold nextOffset) a

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