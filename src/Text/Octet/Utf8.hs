{-# LANGUAGE MagicHash #-}

module Text.Octet.Utf8 where

import Text.Octet.Type
-- import Text.Octet.Bytes
import Data.Bits
import Data.List.NonEmpty qualified as NE
import GHC.Exts
import Data.Word
import GHC.Word
import Data.Functor
import Control.Monad
import Data.Bifunctor

data EncUtf8

type Utf8 = Octet EncUtf8

type Utf8Slice = OctetSlice EncUtf8

{-# INLINE safe #-}
safe :: Char -> Char
safe c
    | isSafe c = c
    | otherwise = '\xfffd'

{-# INLINE isSafe #-}
isSafe :: Char -> Bool
isSafe c = ord c .&. 0x1ff800 /= 0xd800

{-# INLINE utf8Length #-}
utf8Length :: Char -> Int
utf8Length (C# c) = I# ((1# +# geChar# c (chr# 0x80#)) +# (geChar# c (chr# 0x800#) +# geChar# c (chr# 0x10000#)))

{-# INLINE utf8LengthByLeader #-}
utf8LengthByLeader :: Word8 -> Int
utf8LengthByLeader w
  | w < 0x80  = 1
  | w < 0xE0  = 2
  | w < 0xF0  = 3
  | otherwise = 4

{-# INLINE intToWord8 #-}
intToWord8 :: Int -> Word8
intToWord8 = fromIntegral

{-# INLINE ord #-}
ord :: Char -> Int
ord (C# c#) = I# (ord# c#)

-- | Produces up to 4 words. Does not check or verify that the Char is valid
-- utf8.
{-# INLINE toUtf8Bytes #-}
toUtf8Bytes :: Char -> NE.NonEmpty Word8
toUtf8Bytes = \c ->
  let o = ord c
  in case utf8Length c of
      1 -> NE.singleton $ intToWord8 o
      2 -> w1 NE.:| [w2]
        where
        w1 = intToWord8 $ (o `shiftR` 6) + 0xC0
        w2 = intToWord8 $ (o .&. 0x3F)   + 0x80
      3 -> w1 NE.:| [w2, w3]
        where
        w1 = intToWord8 $ (o `shiftR` 12) + 0xE0
        w2 = intToWord8 $ ((o `shiftR` 6) .&. 0x3F) + 0x80
        w3 = intToWord8 $ (o .&. 0x3F) + 0x80
      _4 -> w1 NE.:| [w2, w3, w4]
        where
        w1 = intToWord8 $ (o `shiftR` 18) + 0xF0
        w2 = intToWord8 $ ((o `shiftR` 12) .&. 0x3F) + 0x80
        w3 = intToWord8 $ ((o `shiftR` 6) .&. 0x3F) + 0x80
        w4 = intToWord8 $ (o .&. 0x3F) + 0x80

-- | If the list is more than 4 bytes, we ignore the rest of them.
{-# INLINE fromUtf8Bytes #-}
fromUtf8Bytes :: NE.NonEmpty Word8 -> Char
fromUtf8Bytes = \case
  w1 NE.:| [] -> C# (chr# i1)
    where
    i1 = word8ToInt w1
  w1 NE.:| [w2] -> C# (chr# (c1 +# c2))
    where
    i1 = word8ToInt w1
    i2 = word8ToInt w2
    c1 = uncheckedIShiftL# (i1 -# 0xC0#) 6#
    c2 = i2 -# 0x80#
  w1 NE.:| [w2, w3] -> C# (chr# (c1 +# c2 +# c3))
    where
    i1 = word8ToInt w1
    i2 = word8ToInt w2
    i3 = word8ToInt w3
    c1 = uncheckedIShiftL# (i1 -# 0xE0#) 12#
    c2 = uncheckedIShiftL# (i2 -# 0x80#) 6#
    c3 = i3 -# 0x80#
  w1 NE.:| w2:w3:w4:_ -> C# (chr# (c1 +# c2 +# c3 +# c4))
    where
    i1 = word8ToInt w1
    i2 = word8ToInt w2
    i3 = word8ToInt w3
    i4 = word8ToInt w4
    c1 = uncheckedIShiftL# (i1 -# 0xF0#) 18#
    c2 = uncheckedIShiftL# (i2 -# 0x80#) 12#
    c3 = uncheckedIShiftL# (i3 -# 0x80#) 6#
    c4 = i4 -# 0x80#
  where
  word8ToInt (W8# w8#) = word2Int# (word8ToWord# w8#)

-- | Turn a string into auUtf8 encoded string, converting invalid scalar `Char`s
-- into the replacement character '\xfffd'.
{-# INLINE fromStringReplace #-}
fromStringReplace :: OctetLike o => String -> o EncUtf8
fromStringReplace = Text.Octet.Type.fromListWith (Just . toUtf8Bytes . safe)

-- | Turn a string into a utf8 encoded string, dropping invalid scalar `Char`s.
{-# INLINE fromStringDrop #-}
fromStringDrop :: OctetLike o => String -> o EncUtf8
fromStringDrop = Text.Octet.Type.fromListWith (\c -> guard (isSafe c) $> toUtf8Bytes c)

-- | Fold over a utf8 encoded string, turning each character into a `Char` and
-- applying a right fold.
{-# INLINE foldrUtf8 #-}
foldrUtf8 :: OctetLike o => (Char -> b -> b) -> b -> o EncUtf8 -> b
foldrUtf8 = foldrWith utf8LengthByLeader fromUtf8Bytes

instance Show Utf8 where
  showsPrec i = showsPrec i . foldrUtf8 (:) []

instance Show Utf8Slice where
  showsPrec i = showsPrec i . foldrUtf8 (:) []

instance Read Utf8 where
  readsPrec i s = first fromStringReplace <$> readsPrec i s

instance Read Utf8Slice where
  readsPrec i s = first fromStringReplace <$> readsPrec i s
