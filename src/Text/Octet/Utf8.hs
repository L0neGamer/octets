{-# LANGUAGE MagicHash #-}

module Text.Octet.Utf8 where

import Text.Octet.Type
-- import Text.Octet.Bytes
import Data.Bits
import Data.List.NonEmpty qualified as NE
import GHC.Exts
import Data.Word

data EncUtf8

type Utf8 = Octet EncUtf8

type Utf8Slice = OctetSlice EncUtf8

-- pack :: String -> Utf8
-- pack 

safe :: Char -> Char
safe c
    | ord c .&. 0x1ff800 /= 0xd800 = c
    | otherwise                    = '\xfffd'

utf8Length :: Char -> Int
utf8Length (C# c) = I# ((1# +# geChar# c (chr# 0x80#)) +# (geChar# c (chr# 0x800#) +# geChar# c (chr# 0x10000#)))

intToWord8 :: Int -> Word8
intToWord8 = fromIntegral

ord :: Char -> Int
ord (C# c#) = I# (ord# c#)
{-# INLINE ord #-}

{-# INLINE toUtf8BytesReplace #-}
toUtf8BytesReplace :: Char -> NE.NonEmpty Word8
toUtf8BytesReplace = \c ->
  let c' = safe c
      o = ord c'
  in case utf8Length c' of
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

{-# INLINE fromStringReplace #-}
fromStringReplace :: OctetLike o => String -> o EncUtf8
fromStringReplace = Text.Octet.Type.fromListWith (Just . toUtf8BytesReplace)


  
-- -- | Safe conversion - bytes encoding is as raw as it gets.
-- toBytes :: OctetLike o => o enc -> o EncBytes
-- toBytes = betweenOctets

-- -- | Unsafe conversion from bytes - no guarantee that the otherside will make sense.
-- unsafeFromBytes :: OctetLike o => o EncBytes -> o enc
-- unsafeFromBytes = betweenOctets
