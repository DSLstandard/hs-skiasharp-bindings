module Main where

import Data.Foldable
import Data.Maybe
import Data.Proxy
import Data.Vector qualified as Vector
import Foreign
import GHC.TypeLits
import Linear
import Linear.V qualified
import Skia.Bindings
import Skia.Internal.Utils
import Skia.SKColorType qualified as SKColorType
import Skia.Types
import Skia.Types.Color qualified as Color
import Skia.Types.ColorMatrix qualified as ColorMatrix
import Test.Tasty
import Test.Tasty.HUnit qualified as H
import Test.Tasty.QuickCheck qualified as QC

genV1 :: QC.Gen a -> QC.Gen (V1 a)
genV1 el = V1 <$> el

genV2 :: QC.Gen a -> QC.Gen (V2 a)
genV2 el = V2 <$> el <*> el

genV3 :: QC.Gen a -> QC.Gen (V3 a)
genV3 el = V3 <$> el <*> el <*> el

genV4 :: QC.Gen a -> QC.Gen (V4 a)
genV4 el = V4 <$> el <*> el <*> el <*> el

genV :: forall n a. (KnownNat n) => QC.Gen a -> QC.Gen (Linear.V.V n a)
genV el = do
    vector <- Vector.replicateM (fromIntegral $ natVal (Proxy @n)) el
    pure $ fromJust $ Linear.V.fromVector vector

genRGBA :: QC.Gen a -> QC.Gen (RGBA a)
genRGBA el = RGBA <$> el <*> el <*> el <*> el

test'InternalUtils :: TestTree
test'InternalUtils =
    testGroup
        "Skia.Internal.Utils"
        [ H.testCase "convert4Word8ToWord32 examples" do
            0xDEADBEEF H.@=? convert4Word8ToWord32 (0xDE, 0xAD, 0xBE, 0xEF)
        , QC.testProperty "convert4Word8ToWord32 . convertWord32To4Word8 == id" do
            value <- QC.arbitrary @Word32
            pure $ convert4Word8ToWord32 (convertWord32To4Word8 value) == value
        ]

test'ColorMatrix :: TestTree
test'ColorMatrix =
    testGroup
        "Skia.Types.ColorMatrix"
        [ H.testCase "fromColumns (toColumns identity) == identity" do
            ColorMatrix.identity H.@=? ColorMatrix.fromColumns (ColorMatrix.toColumns (ColorMatrix.identity @Float))
        , QC.testProperty "toColumns . fromColumns == id" do
            c1 <- genV4 QC.arbitrary
            c2 <- genV4 QC.arbitrary
            c3 <- genV4 QC.arbitrary
            c4 <- genV4 QC.arbitrary
            c5 <- genV4 QC.arbitrary
            let fiveColumns = (c1, c2, c3, c4, c5)
            pure $ ColorMatrix.toColumns (ColorMatrix.fromColumns @Float fiveColumns) == fiveColumns
        , H.testCase "fromM45 (toM45 identity) == identity" do
            ColorMatrix.identity H.@=? ColorMatrix.fromM45 (ColorMatrix.toM45 (ColorMatrix.identity @Float))
        , QC.testProperty "toM45 . fromM45 == id" do
            m45 <- genV4 (genV QC.arbitrary)
            pure $ ColorMatrix.toM45 (ColorMatrix.fromM45 @Float m45) == m45
        ]

test'Color :: TestTree
test'Color =
    testGroup
        "Skia.Types.Color"
        [ H.testCase "toSKColor examples" do
            SKColor 0xFF000000 H.@?= Color.toSKColor (RGBA 0x00 0x00 0x00 0xFF)
            SKColor 0x44556677 H.@?= Color.toSKColor (RGBA 0x55 0x66 0x77 0x44)
        , QC.testProperty "fromSKColor . toSKColor == id" do
            rgba <- genRGBA QC.arbitrary
            pure $ Color.fromSKColor (Color.toSKColor rgba) == rgba
        , QC.testProperty "fromSKColor4f . toSKColor4f == id" do
            rgba <- genRGBA QC.arbitrary
            pure $ Color.fromSKColor4f (Color.toSKColor4f rgba) == rgba
        ]

test'SKColorType :: TestTree
test'SKColorType =
    testGroup
        "Skia.SKColorType"
        [ H.testCase "bytesPerPixel correctness" do
            for_ [minBound .. maxBound] \(colortype :: SKColorType) -> do
                expected <- hsskia_SkColorTypeBytesPerPixel (marshalSKEnum colortype)
                let got = SKColorType.bytesPerPixel colortype
                H.assertEqual ("input = " <> show colortype) expected (fromIntegral got)
        ]

mainTestTree :: TestTree
mainTestTree =
    testGroup
        "Skia tests"
        [ test'InternalUtils
        , test'ColorMatrix
        , test'Color
        , test'SKColorType
        ]

main :: IO ()
main = defaultMain mainTestTree
