module Skia.Types.Internal.Utils.Extra where

import Control.Exception qualified
import Control.Monad
import Control.Monad.Cont
import Control.Monad.IO.Class
import Data.Coerce
import Data.Functor
import Data.Maybe
import Foreign
import Foreign.C.Types
import Skia.Bindings
import Skia.Internal.Utils
import Skia.Types.Core
import Skia.Types.Enums
import Skia.Types.Extra
import Skia.Types.Internal.Utils.Core
import Skia.Types.Rect qualified as Rect

{- | Skottie_resource_provider and Skresources_resource_provider are actually
the same thing if you look into Google Skia's source code.
-}
coerceToSkottieResourceProviderPtr :: Ptr Skresources_resource_provider -> Ptr Skottie_resource_provider
coerceToSkottieResourceProviderPtr = castPtr

unmarshalSKImageInfo :: (MonadIO m) => Sk_imageinfo -> m SKImageInfo
unmarshalSKImageInfo iminfo = do
    colorspace <- toObject iminfo.colorspace
    colorType <- unmarshalSKEnumOrDie iminfo.colorType
    alphaType <- unmarshalSKEnumOrDie iminfo.alphaType
    pure
        SKImageInfo
            { colorspace = colorspace
            , width = fromIntegral iminfo.width
            , height = fromIntegral iminfo.height
            , colorType = colorType
            , alphaType = alphaType
            }

useSKImageInfo :: SKImageInfo -> ContT r IO (Ptr Sk_imageinfo)
useSKImageInfo iminfo = do
    colorspace' <- useObj iminfo.colorspace
    useStorable $
        Sk_imageinfo
            { colorspace = colorspace'
            , width = fromIntegral iminfo.width
            , height = fromIntegral iminfo.height
            , colorType = marshalSKEnum iminfo.colorType
            , alphaType = marshalSKEnum iminfo.alphaType
            }

marshalSKCubicResampler :: SKCubicResampler -> Sk_cubic_resampler
marshalSKCubicResampler i =
    Sk_cubic_resampler
        { fB = coerce i.b
        , fC = coerce i.c
        }

marshalSKSamplingOptions :: SKSamplingOptions -> Sk_sampling_options
marshalSKSamplingOptions i =
    Sk_sampling_options
        { fMaxAniso = fromIntegral i.maxAniso
        , fUseCubic = fromBool $ isJust i.cubicResampler
        , fCubic = maybe dummyCubicResampler marshalSKCubicResampler i.cubicResampler
        , fFilter = marshalSKEnum i.filterMode
        , fMipmap = marshalSKEnum i.mipmapMode
        }
  where
    dummyCubicResampler = Sk_cubic_resampler{fB = 0, fC = 0}

marshalSKPngEncoderFilterFlags :: SKPngEncoderFilterFlags -> Sk_pngencoder_filterflags
marshalSKPngEncoderFilterFlags flags =
    bitOrs $
        mapMaybe
            (\(flagBool, flagBit) -> guard flagBool $> flagBit)
            [ (flags.none, NONE_SK_PNGENCODER_FILTER_FLAGS)
            , (flags.sub, SUB_SK_PNGENCODER_FILTER_FLAGS)
            , (flags.up, UP_SK_PNGENCODER_FILTER_FLAGS)
            , (flags.avg, AVG_SK_PNGENCODER_FILTER_FLAGS)
            , (flags.paeth, PAETH_SK_PNGENCODER_FILTER_FLAGS)
            ]

useSKPngEncoderOptions :: SKPngEncoderOptions -> ContT r IO (Ptr Sk_pngencoder_options)
useSKPngEncoderOptions opts = do
    iccProfile' <- useNullIfNothing useObj opts.iccProfile
    iccProfileDescription' <- useNullIfNothing useTextAsUtf8CString opts.iccProfileDescription
    useStorable
        Sk_pngencoder_options
            { fFilterFlags = marshalSKPngEncoderFilterFlags opts.filterFlags
            , fZLibLevel = fromIntegral opts.zlibLevel
            , fComments = fromMaybe nullPtr opts.comments
            , fICCProfile = iccProfile'
            , fICCProfileDescription = iccProfileDescription'
            }

useSKJpegEncoderOptions :: SKJpegEncoderOptions -> ContT r IO (Ptr Sk_jpegencoder_options)
useSKJpegEncoderOptions opts = do
    iccProfile' <- useNullIfNothing useObj opts.iccProfile
    iccProfileDescription' <- useNullIfNothing useTextAsUtf8CString opts.iccProfileDescription
    xmpMetadata' <- useNullIfNothing useObj opts.xmpMetadata
    useStorable
        Sk_jpegencoder_options
            { fQuality = fromIntegral opts.quality
            , fDownsample = marshalSKEnum opts.downsample
            , fAlphaOption = marshalSKEnum opts.alphaOption
            , xmpMetadata = xmpMetadata'
            , fICCProfile = iccProfile'
            , fICCProfileDescription = iccProfileDescription'
            }

defaultSKWebpEncoderOptions :: SKWebpEncoderOptions
defaultSKWebpEncoderOptions =
    SKWebpEncoderOptions
        { compression = SKWebpEncoderCompression'Lossy
        , quality = 100.0
        , iccProfile = Nothing
        , iccProfileDescription = Nothing
        }

useSKWebpEncoderOptions :: SKWebpEncoderOptions -> ContT r IO (Ptr Sk_webpencoder_options)
useSKWebpEncoderOptions opts = evalContIO do
    iccProfile' <- useNullIfNothing useObj opts.iccProfile
    iccProfileDescription' <- useNullIfNothing useTextAsUtf8CString opts.iccProfileDescription
    useStorable
        Sk_webpencoder_options
            { fCompression = marshalSKEnum opts.compression
            , fQuality = coerce opts.quality
            , fICCProfile = iccProfile'
            , fICCProfileDescription = iccProfileDescription'
            }

useSKCodecOptions :: SKCodecOptions -> ContT r IO (Ptr Sk_codec_options)
useSKCodecOptions input = do
    subset <- useStorable $ Rect.toSKIRect input.subset
    useStorable
        Sk_codec_options
            { fZeroInitialized = fromBool input.isZeroInitialized
            , fSubset = subset
            , fFrameIndex = fromIntegral input.frameIndex
            , fPriorFrame = fromIntegral input.priorFrame
            }

-- | NOTE: This is 'IO' solely because this function uses 'unmarshalSKEnumOrDie'.
unmarshalSKCodecFrameInfo :: (MonadIO m) => Sk_codec_frameinfo -> m SKCodecFrameInfo
unmarshalSKCodecFrameInfo input = liftIO do
    let requiredFrame = if input.fRequiredFrame == -1 then Nothing else Just (fromIntegral input.fRequiredFrame)
    let durationMs = fromIntegral input.fDuration
    let fullyReceived = toBool input.fFullyReceived

    alphaType <- unmarshalSKEnumOrDie input.fAlphaType
    let hasAlphaWithinBounds = toBool input.fHasAlphaWithinBounds

    disposalMethod <- unmarshalSKEnumOrDie input.fDisposalMethod
    blend <- unmarshalSKEnumOrDie input.fBlend

    let frameRect0 = Rect.fromSKIRect input.fFrameRect
    let frameRect = if Rect.isEmpty frameRect0 then Nothing else Just frameRect0

    pure SKCodecFrameInfo{..}

peekSKFontStyle :: (MonadIO m) => Ptr Sk_fontstyle -> m SKFontStyle
peekSKFontStyle ptr = liftIO do
    weight <- fmap fromIntegral $ sk_fontstyle_get_weight ptr
    width <- fmap fromIntegral $ sk_fontstyle_get_width ptr
    slant <- unmarshalSKEnumOrDie =<< sk_fontstyle_get_slant ptr
    pure SKFontStyle{..}

useSKFontStyle :: SKFontStyle -> ContT r IO (Ptr Sk_fontstyle)
useSKFontStyle style =
    ContT $
        Control.Exception.bracket
            ( sk_fontstyle_new
                (fromIntegral style.weight)
                (fromIntegral style.width)
                (marshalSKEnum style.slant)
            )
            sk_fontstyle_delete

-- | Used to allocate an empty 'Sk_fontstyle' as a destination buffer.
useAllocaSKFontStyle :: ContT r IO (Ptr Sk_fontstyle)
useAllocaSKFontStyle = do
    -- The only way to create SKFontStyle that Mono Skia provides is through
    -- sk_fontstyle_new, which requires specifying some dummy weight/width/slant
    -- values. SkiaSharp puts fontStyle'Normal. We will follow.
    let dummy = fontStyle'Normal
    useSKFontStyle dummy

peekSKSurfaceProps :: (MonadIO m) => Ptr Sk_surfaceprops -> m SKSurfaceProps
peekSKSurfaceProps props' = liftIO do
    flags <- Sk_surfaceprops_flags <$> sk_surfaceprops_get_flags props'

    let usesDeviceIndependentFonts = hasFlag USE_DEVICE_INDEPENDENT_FONTS_SK_SURFACE_PROPS_FLAGS flags
    let usesDynamicMSAA = hasFlag DYNAMIC_MSAA_SK_SURFACE_PROPS_FLAGS flags
    let alwaysDither = hasFlag ALWAYS_DITHER_SK_SURFACE_PROPS_FLAGS flags

    pixelGeometry <- unmarshalSKEnumOrDie =<< sk_surfaceprops_get_pixel_geometry props'
    pure SKSurfaceProps{..}

useSKSurfaceProps :: SKSurfaceProps -> ContT r IO (Ptr Sk_surfaceprops)
useSKSurfaceProps props =
    ContT $
        Control.Exception.bracket
            (sk_surfaceprops_new flags pixelGeometry)
            sk_surfaceprops_delete
  where
    pixelGeometry = marshalSKEnum props.pixelGeometry

    Sk_surfaceprops_flags flags =
        makeBitFlags
            [ (props.usesDeviceIndependentFonts, USE_DEVICE_INDEPENDENT_FONTS_SK_SURFACE_PROPS_FLAGS)
            , (props.usesDynamicMSAA, DYNAMIC_MSAA_SK_SURFACE_PROPS_FLAGS)
            , (props.alwaysDither, ALWAYS_DITHER_SK_SURFACE_PROPS_FLAGS)
            ]

-- | Used to allocate an empty 'Sk_surfaceprops' as a destination buffer.
useAllocaSKSurfaceProps :: ContT r IO (Ptr Sk_surfaceprops)
useAllocaSKSurfaceProps = ContT do
    Control.Exception.bracket
        (sk_surfaceprops_new 0 0) -- Bogus values.
        sk_surfaceprops_delete
