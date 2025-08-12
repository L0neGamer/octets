{-# LANGUAGE RequiredTypeArguments #-}

module Text.Octet.Utf8Spec (spec_utf8_roundtrip) where

import Test.Hspec
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Text.Octet.Utf8 as Utf8
import Text.Octet.Octet as Octet
import Text.Octet.OctetSlice as OctetSlice
import Test.Hspec.Hedgehog ()
import Data.Kind
import Text.Read

spec_utf8_roundtrip :: Spec
spec_utf8_roundtrip = do
  describe "with unicode" $ do
    describe "fromStringReplace" $ testWithBoth $ generalRoundTrip Gen.unicode id Utf8.fromStringReplace
    describe "fromStringDrop" $ testWithBoth $ generalRoundTrip Gen.unicode id Utf8.fromStringDrop
  describe "with unicodeAll" $ do
    describe "fromStringReplace" $ testWithBoth $ generalRoundTrip Gen.unicodeAll (fmap safe) Utf8.fromStringReplace
    describe "fromStringDrop" $ testWithBoth $ generalRoundTrip Gen.unicodeAll (filter isSafe) Utf8.fromStringDrop

  describe "read" $ do
    describe "with unicode" $ testWithBoth $ readRoundTrip Gen.unicode id
    describe "with unicodeAll" $ testWithBoth $ readRoundTrip Gen.unicodeAll (fmap safe)
  where
  generalRoundTrip :: Gen Char -> (String -> String) -> (forall o' . OctetLike o' => String -> o' EncUtf8) -> forall (o :: Type -> Type) -> (OctetLike o, Read (o EncUtf8)) => PropertyT IO ()
  generalRoundTrip charGen modifyString fromString' o = do
    str <- forAll $ Gen.string (Range.linear 0 100) charGen
    toString (fromString' @o str) === modifyString str

  readRoundTrip :: Gen Char -> (String -> String) -> forall (o :: Type -> Type) -> (OctetLike o, Read (o EncUtf8)) => PropertyT IO ()
  readRoundTrip charGen modifyString o = do
    str <- forAll $ Gen.string (Range.linear 0 100) charGen
    let showStr = show str
    (toString <$> readMaybe @(o EncUtf8) showStr) === (modifyString <$> readMaybe showStr)

testWithBoth :: (forall o -> (OctetLike o, Read (o EncUtf8)) => PropertyT IO ()) -> Spec
testWithBoth testWith = do
  it "Utf8" $ testWith Octet.Octet
  it "Utf8Slice" $ testWith OctetSlice.OctetSlice

