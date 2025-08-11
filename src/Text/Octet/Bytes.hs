
module Text.Octet.Bytes where

import Text.Octet.Type
import Data.Word

data EncBytes

type Bytes = Octet EncBytes

type BytesSlice = OctetSlice EncBytes

-- | Safe conversion - bytes encoding is as raw as it gets.
toBytes :: OctetLike o => o enc -> o EncBytes
toBytes = coerceOctet

-- | Unsafe conversion from bytes - no guarantee that the otherside will make sense.
unsafeFromBytes :: OctetLike o => o EncBytes -> o enc
unsafeFromBytes = coerceOctet

fromList :: OctetLike o => [Word8] -> o EncBytes
fromList = Text.Octet.Type.fromListWith (Just . pure)
