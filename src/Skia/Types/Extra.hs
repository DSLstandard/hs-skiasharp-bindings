module Skia.Types.Extra where

import Control.Monad
import Data.Bits
import Data.Char qualified
import Data.Coerce
import Data.Functor
import Data.Int
import Data.Text qualified as T
import Data.Word
import Foreign
import Foreign.C
import Skia.Bindings
import Skia.Types.Enums
import Skia.Types.Objects
import Skia.Types.Rect

type Degrees = Float
type Radians = Float

data InColor color hex
    = InColor color SKColorSpace
    | InColor'Hex hex
    deriving (Show)

newtype GlyphId = GlyphId {unGlyphId :: Word16}
    deriving (Show, Eq, Ord)
    deriving newtype (Num, Storable)

data FontMetrics = FontMetrics
    { glyphBounds :: Maybe (Rect Float)
    -- ^ TOP = greatest extent above origin of any glyph bounding box, typically
    -- negative; deprecated with variable fonts
    --
    -- BOTTOM = greatest extent below origin of any glyph bounding box,
    -- typically positive; deprecated with variable fonts
    --
    -- LEFT = (also called xMin) greatest extent to left of origin of any glyph
    -- bounding box, typically negative; deprecated with variable fonts
    --
    -- RIGHT = (also called xMax) greatest extent to right of origin of any
    -- glyph bounding box, typically negative; deprecated with variable fonts
    , ascent :: Float
    -- ^ Distance to reserve above baseline, typically negative.
    , descent :: Float
    -- ^ Distance to reserve below baseline, typically positive.
    , leading :: Float
    -- ^ Distance to add between lines, typically positive or zero.
    , avgCharWidth :: Maybe Float
    -- ^ Average character width, 'Nothing' if unknown.
    , maxCharWidth :: Maybe Float
    -- ^ Maximum character width, 'Nothing' if unknown.
    , xHeight :: Maybe Float
    -- ^ Height of lower-case 'x', typically negative, 'Nothing' if unknown.
    , capHeight :: Maybe Float
    -- ^ Height of an upper-case letter, typically negative, 'Nothing' if unknown,
    , underlineThickness :: Maybe Float
    -- ^ Underline thickness, 'Nothing' if invalid.
    , underlinePosition :: Maybe Float
    -- ^ Distance from baseline to top of stroke, typically positive, 'Nothing' if invalid.
    , strikeoutThickness :: Maybe Float
    -- ^ Strikeout thickness, 'Nothing' if invalid.
    , strikeoutPosition :: Maybe Float
    -- ^ Distance from baseline to bottom of stroke, typically negative, 'Nothing' if invalid.
    }
    deriving (Show)

marshalFontMetrics :: Sk_fontmetrics -> FontMetrics
marshalFontMetrics input =
    FontMetrics
        { glyphBounds = do
            validUnlessFlagBitSet
                kBoundsInvalid_FlagBit
                Rect
                    { left = coerce input.fXMin
                    , right = coerce input.fXMax
                    , top = coerce input.fTop
                    , bottom = coerce input.fBottom
                    }
        , ascent = coerce input.fAscent
        , descent = coerce input.fDescent
        , leading = coerce input.fLeading
        , avgCharWidth = validUnlessZero input.fAvgCharWidth
        , maxCharWidth = validUnlessZero input.fMaxCharWidth
        , xHeight = validUnlessZero input.fXHeight
        , capHeight = validUnlessZero input.fCapHeight
        , underlineThickness = validIfFlagBitSet kUnderlineThicknessIsValid_FlagBit (coerce input.fUnderlineThickness)
        , underlinePosition = validIfFlagBitSet kUnderlinePositionIsValid_FlagBit (coerce input.fUnderlinePosition)
        , strikeoutThickness = validIfFlagBitSet kStrikeoutThicknessIsValid_FlagBit (coerce input.fStrikeoutThickness)
        , strikeoutPosition = validIfFlagBitSet kStrikeoutPositionIsValid_FlagBit (coerce input.fStrikeoutPosition)
        }
  where
    -- Some values are set to "zero" when unknown. Example: Google Skia's
    -- comment on fXHeight: height of lower-case 'x', zero if unknown,
    -- typically negative.
    validUnlessZero :: CFloat -> Maybe Float
    validUnlessZero value =
        -- TODO: It should be fine to simply do (== 0)... right?
        if value == 0
            then Nothing
            else Just (coerce value)

    validIfFlagBitSet :: Int -> a -> Maybe a
    validIfFlagBitSet bit value = guard (testBit input.fFlags bit) $> value

    validUnlessFlagBitSet :: Int -> a -> Maybe a
    validUnlessFlagBitSet bit value = guard (not $ testBit input.fFlags bit) $> value

    -- NOTE: See Google Skia's include/core/SkFontMetrics.h's `enum FontMetricsFlags`
    kUnderlineThicknessIsValid_FlagBit = 0 -- set if fUnderlineThickness is valid
    kUnderlinePositionIsValid_FlagBit = 1 -- set if fUnderlinePosition is valid
    kStrikeoutThicknessIsValid_FlagBit = 2 -- set if fStrikeoutThickness is valid
    kStrikeoutPositionIsValid_FlagBit = 3 -- set if fStrikeoutPosition is valid
    kBoundsInvalid_FlagBit = 4 -- set if fTop, fBottom, fXMin, fXMax invalid

type Unichar = Int32

newtype SKFontTableTag = SKFontTableTag {unFontTableTag :: Sk_font_table_tag}
    deriving (Show, Eq, Ord)

mkFontTableTagBy4Byte :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
mkFontTableTagBy4Byte c1 c2 c3 c4 = do
    -- FIXME: Is there already a builtin function that combines four Word8s into
    -- one Word32?
    let c1' = fromIntegral c1
    let c2' = fromIntegral c2
    let c3' = fromIntegral c3
    let c4' = fromIntegral c4
    (c1' `shiftL` 24) .|. (c2' `shiftL` 16) .|. (c3' `shiftL` 8) .|. c4'

mkFontTableTagByString :: String -> Word32
mkFontTableTagByString [c1, c2, c3, c4] = do
    mkFontTableTagBy4Byte (toByte c1) (toByte c2) (toByte c3) (toByte c4)
  where
    toByte :: Char -> Word8
    toByte ch = fromIntegral (Data.Char.ord ch)
mkFontTableTagByString _ = do
    error "Input string must have exactly 4 chars"

-- About the definition of 'SKSurfaceProps', see Google Skia's
-- include/core/SkSurfaceProps.h

{- | Describes properties and constraints of a given SKSurface. The rendering
engine can parse these during drawing, and can sometimes optimize its
performance (e.g. disabling an expensive feature).
-}
data SKSurfaceProps = SKSurfaceProps
    { usesDeviceIndependentFonts :: Bool
    , usesDynamicMSAA :: Bool
    -- ^ Use internal MSAA to render to non-MSAA GPU surfaces.
    , alwaysDither :: Bool
    -- ^ If set, all rendering will have dithering enabled. Currently this only
    -- impacts GPU backends
    , pixelGeometry :: SKPixelGeometry
    }
    deriving (Show, Eq, Ord)

-- See include/core/SkFontStyle.h
type FontWeight = Int

fontWeight'Invisible :: FontWeight
fontWeight'Invisible = 0

fontWeight'Thin :: FontWeight
fontWeight'Thin = 100

fontWeight'ExtraLight :: FontWeight
fontWeight'ExtraLight = 200

fontWeight'Light :: FontWeight
fontWeight'Light = 300

fontWeight'Normal :: FontWeight
fontWeight'Normal = 400

fontWeight'Medium :: FontWeight
fontWeight'Medium = 500

fontWeight'SemiBold :: FontWeight
fontWeight'SemiBold = 600

fontWeight'Bold :: FontWeight
fontWeight'Bold = 700

fontWeight'ExtraBold :: FontWeight
fontWeight'ExtraBold = 800

fontWeight'Black :: FontWeight
fontWeight'Black = 900

fontWeight'ExtraBlack :: FontWeight
fontWeight'ExtraBlack = 1000

-- See include/core/SkFontStyle.h
type FontWidth = Int

fontWidth'UltraCondensed :: FontWidth
fontWidth'UltraCondensed = 1

fontWidth'ExtraCondensed :: FontWidth
fontWidth'ExtraCondensed = 2

fontWidth'Condensed :: FontWidth
fontWidth'Condensed = 3

fontWidth'SemiCondensed :: FontWidth
fontWidth'SemiCondensed = 4

fontWidth'Normal :: FontWidth
fontWidth'Normal = 5

fontWidth'SemiExpanded :: FontWidth
fontWidth'SemiExpanded = 6

fontWidth'Expanded :: FontWidth
fontWidth'Expanded = 7

fontWidth'ExtraExpanded :: FontWidth
fontWidth'ExtraExpanded = 8

fontWidth'UltraExpanded :: FontWidth
fontWidth'UltraExpanded = 9

{-
Google Skia's SKFontStyle is a very cheap structure - a single int32_t encoding
weight, width, and slant.

To see the definition of the data fields, see Google Skia's
include/core/SkFontStyle.h

This datatype only holds what is decoded values.

We use 'peekSKFontStyle', 'useSKFontStyle', and 'useAllocaSKFontStyle' to do FFI
for Skia functions involving 'Sk_fontstyle'.
-}
data SKFontStyle = SKFontStyle
    { weight :: FontWeight
    , width :: FontWidth
    , slant :: SKFontStyleSlant
    }
    deriving (Show, Eq, Ord)

fontStyle'Normal :: SKFontStyle
fontStyle'Normal =
    SKFontStyle
        { weight = fontWeight'Normal
        , width = fontWidth'Normal
        , slant = SKFontStyleSlant'Upright
        }

fontStyle'Bold :: SKFontStyle
fontStyle'Bold =
    SKFontStyle
        { weight = fontWeight'Bold
        , width = fontWidth'Normal
        , slant = SKFontStyleSlant'Upright
        }

fontStyle'Italic :: SKFontStyle
fontStyle'Italic =
    SKFontStyle
        { weight = fontWeight'Normal
        , width = fontWidth'Normal
        , slant = SKFontStyleSlant'Italic
        }

fontStyle'BoldItalic :: SKFontStyle
fontStyle'BoldItalic =
    SKFontStyle
        { weight = fontWeight'Bold
        , width = fontWidth'Normal
        , slant = SKFontStyleSlant'Italic
        }

data SKCodecFrameInfo = SKCodecFrameInfo
    { requiredFrame :: Maybe Int
    -- ^ The frame that this frame needs to be blended with, or 'Nothing' if
    -- this frame is independent (so it can be drawn over an uninitialized
    -- buffer).
    --
    -- Note that this is the *earliest* frame that can be used for blending. Any
    -- frame from ['requiredFrame', i) can be used, unless its 'disposalMethod'
    -- is 'SKCodecAnimationDisposalMethod\'RestorePrevious'.
    , durationMs :: Int
    -- ^ Number of milliseconds to show this frame.
    , fullyReceived :: Bool
    -- ^ Whether the end marker for this frame is contained in the stream.
    --
    -- Note: this does not guarantee that an attempt to decode will be complete.
    -- There could be an error in the stream.
    , alphaType :: SKAlphaType
    -- ^ This is conservative; it will still return non-opaque if e.g. a color
    -- index-based frame has a color with alpha but does not use it.
    , hasAlphaWithinBounds :: Bool
    -- ^ Whether the updated rectangle contains alpha.
    --
    -- This is conservative; it will still be set to true if e.g. a color
    -- index-based frame has a color with alpha but does not use it. In
    -- addition, it may be set to true, even if the final frame, after blending,
    -- is opaque.
    , disposalMethod :: SKCodecAnimationDisposalMethod
    -- ^ How this frame should be modified before decoding the next one.
    , blend :: SKCodecAnimationBlend
    -- ^ How this frame should blend with the prior frame.
    , frameRect :: Maybe (Rect Int)
    -- ^ The rectangle updated by this frame.
    --
    -- It may be empty (represented by 'Nothing'), if the frame does not change
    -- the image. It will always be contained by dimensions of the codec.
    }
    deriving (Show, Eq)

data SKCodecOptions = SKCodecOptions
    { isZeroInitialized :: Bool
    , subset :: Rect Int
    , frameIndex :: Int
    , priorFrame :: Int
    }
    deriving (Show, Eq, Ord)

data SKWebpEncoderOptions = SKWebpEncoderOptions
    { compression :: SKWebpEncoderCompression
    , quality :: Float
    , iccProfile :: Maybe SKColorSpaceICCProfile
    , iccProfileDescription :: Maybe T.Text
    }
    deriving (Show)

data SKJpegEncoderOptions = SKJpegEncoderOptions
    { quality :: Int
    , downsample :: SKJpegEncoderDownsample
    , alphaOption :: SKJpegEncoderAlphaOption
    , xmpMetadata :: Maybe SKData
    , iccProfile :: Maybe SKColorSpaceICCProfile
    , iccProfileDescription :: Maybe T.Text
    }
    deriving (Show)

defaultSKJpegEncoderOptions :: SKJpegEncoderOptions
defaultSKJpegEncoderOptions =
    SKJpegEncoderOptions
        { quality = 100
        , downsample = SKJpegEncoderDownsample'Downsample420
        , alphaOption = SKJpegEncoderAlphaOption'Ignore
        , xmpMetadata = Nothing
        , iccProfile = Nothing
        , iccProfileDescription = Nothing
        }

data SKPngEncoderFilterFlags = SKPngEncoderFilterFlags
    { none :: Bool
    , sub :: Bool
    , up :: Bool
    , avg :: Bool
    , paeth :: Bool
    }
    deriving (Show, Eq, Ord)

data SKPngEncoderOptions = SKPngEncoderOptions
    { filterFlags :: SKPngEncoderFilterFlags
    , zlibLevel :: Int
    , comments :: Maybe (Ptr ())
    -- ^ TODO: What does this do?
    , iccProfile :: Maybe SKColorSpaceICCProfile
    , iccProfileDescription :: Maybe T.Text
    }
    deriving (Show)

defaultSKPngEncoderOptions :: SKPngEncoderOptions
defaultSKPngEncoderOptions =
    SKPngEncoderOptions
        { filterFlags
        , zlibLevel = 6
        , comments = Nothing
        , iccProfile = Nothing
        , iccProfileDescription = Nothing
        }
  where
    filterFlags = SKPngEncoderFilterFlags{none = True, sub = True, up = True, avg = True, paeth = True}

data SKSamplingOptions = SKSamplingOptions
    { maxAniso :: Int
    , cubicResampler :: Maybe SKCubicResampler
    , filterMode :: SKFilterMode
    , mipmapMode :: SKMipmapMode
    }
    deriving (Show, Eq, Ord)

data SKCubicResampler = SKCubicResampler
    { b :: Float
    , c :: Float
    }
    deriving (Show, Eq, Ord)

{- | Describes pixel dimensions and encoding. 'SKBitmap', 'SKImage', 'SKPixmap',
and 'SKSurface' can be created from 'SKImageInfo'.

'SKImageInfo' can be retrieved from 'SKBitmap' and 'SKPixmap', but not from
'SKImage' and 'SKSurface'. For example, 'SKImage' and 'SKSurface'
implementations may defer pixel depth, so may not completely specify
'SKImageInfo'.

'SKImageInfo' contains dimensions, the pixel integral width and height. It
encodes how pixel bits describe alpha, transparency; color components red, blue,
and green; and 'SKColorSpace', the range and linearity of colors.
-}
data SKImageInfo = SKImageInfo
    { colorspace :: SKColorSpace -- TODO: Use 'Maybe' to allow 'nullPtr'?
    , width :: Int
    , height :: Int
    , colorType :: SKColorType
    , alphaType :: SKAlphaType
    }
    deriving (Show)

{- | Annotation type for documentation.

You should only encounter this type in the return type of a function (typically
an object creation/factory function). The type @a@ is ALWAYS a subclass of
either 'SKNVRefCnt' or 'SKRefCnt'; otherwise, it is a documentation error.

When you see:

@
createFoo :: ... -> m (Ref a)
@

(or variants such as @createFoo :: ... -> m (Maybe (Ref a))@), you do not need
to worry about the need to manually decrement the SKNVRefCnt/SKRefCnt reference
counter of the object once you are done using it. This Haskell library automates
the process - the decrementation is performed automatically by the Haskell
runtime when the object goes out-of-scope and is finalized.
-}
type Ref a = a

{- | Annotation type for documentation.

You should only encounter this type in the return type of a function (typically
an object creation/factory function).

When you see:

@
createFoo :: ... -> m (Owned a)
@

(or variants such as @createFoo :: ... -> m (Maybe (Owned a))@), you, as the
user of this library, **ARE** responsible for destroying the object once you are
done using it using functions typically named @destroy@.

If the object goes out-of-scope and it is not destroyed, it is a memory leak.
-}
type Owned a = a