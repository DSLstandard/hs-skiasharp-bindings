module Skia.SKColorFilter where

import Control.Exception qualified
import Data.Vector.Storable qualified as VS
import Skia.Internal.Prelude
import Skia.Types.ColorMatrix qualified as ColorMatrix

createBlend :: (MonadIO m) => SKColor -> SKBlendMode -> m SKColorFilter
createBlend color blendMode = evalContIO do
    result' <- liftIO $ sk_colorfilter_new_mode (coerce color) (marshalSKEnum blendMode)
    toObjectFin sk_colorfilter_unref result'

{- | Create a colorfilter that multiplies the RGB channels by one color, and
then adds a second color, pinning the result for each component to [0..255].
The alpha components of the mul and add arguments are ignored.
-}
createLighting ::
    (MonadIO m) =>
    -- | \"mul\"
    SKColor ->
    -- | \"add\"
    SKColor ->
    m SKColorFilter
createLighting mul add = liftIO do
    result' <- liftIO $ sk_colorfilter_new_lighting (coerce mul) (coerce add)
    toObjectFin sk_colorfilter_unref result'

createCompose ::
    (MonadIO m) =>
    -- | Outer
    SKColorFilter ->
    -- | Inner
    SKColorFilter ->
    m SKColorFilter
createCompose outer inner = evalContIO do
    outer' <- useObj outer
    inner' <- useObj inner

    result' <- liftIO $ sk_colorfilter_new_compose outer' inner'
    toObjectFin sk_colorfilter_unref result'

createColorMatrix ::
    (MonadIO m) =>
    ColorMatrix Float ->
    m SKColorFilter
createColorMatrix matrix = evalContIO do
    result' <- liftIO $ ColorMatrix.unsafeWith matrix \array' -> do
        sk_colorfilter_new_color_matrix (castPtr array')

    -- FIXME: See src/effects/colorfilters/SkMatrixColorFilter.cpp. The returned
    -- color filter could be nullptr if the input matrix contains infinity.
    toObjectFin sk_colorfilter_unref result'

createHslaMatrix ::
    (MonadIO m) =>
    ColorMatrix Float ->
    m SKColorFilter
createHslaMatrix matrix = evalContIO do
    result' <- liftIO $ ColorMatrix.unsafeWith matrix \array' -> do
        sk_colorfilter_new_hsla_matrix (castPtr array')

    -- FIXME: See src/effects/colorfilters/SkMatrixColorFilter.cpp. The returned
    -- color filter could be nullptr if the input matrix contains infinity.
    toObjectFin sk_colorfilter_unref result'

createLinearToSrgbGamma :: (MonadIO m) => m SKColorFilter
createLinearToSrgbGamma = liftIO do
    result' <- sk_colorfilter_new_linear_to_srgb_gamma
    toObjectFin sk_colorfilter_unref result'

createSrgbToLinearGamma :: (MonadIO m) => m SKColorFilter
createSrgbToLinearGamma = liftIO do
    result' <- sk_colorfilter_new_srgb_to_linear_gamma
    toObjectFin sk_colorfilter_unref result'

-- TODO: t = 0 => dest/src? Need to confirm.
createLerp ::
    (MonadIO m) =>
    -- | t
    Float ->
    -- | Destination
    SKColorFilter ->
    -- | Source
    SKColorFilter ->
    m SKColorFilter
createLerp t dst src = evalContIO do
    dst' <- useObj dst
    src' <- useObj src
    result' <- liftIO $ sk_colorfilter_new_lerp (coerce t) dst' src'
    toObjectFin sk_colorfilter_unref result'

createLumaColor :: (MonadIO m) => m SKColorFilter
createLumaColor = liftIO do
    result' <- sk_colorfilter_new_luma_color
    toObjectFin sk_colorfilter_unref result'

createHighContrast ::
    (MonadIO m) =>
    -- | Grayscale?
    Bool ->
    SKHighContrastConfigInvertStyle ->
    -- | Contrast
    Float ->
    m SKColorFilter
createHighContrast grayscale invertStyle contrast = evalContIO do
    conf' <-
        useStorable
            Sk_highcontrastconfig
                { fGrayscale = fromBool grayscale
                , fInvertStyle = marshalSKEnum invertStyle
                , fContrast = coerce contrast
                }

    result' <- liftIO $ sk_colorfilter_new_high_contrast conf'
    toObjectFin sk_colorfilter_unref result'

-- | Throws an error if size of the lookup table is not 256.
createTable ::
    (MonadIO m) =>
    -- | Color value lookup table of type (Word8 -> Word8) encoded as a
    -- 256-length vector.
    VS.Vector Word8 ->
    m SKColorFilter
createTable table = evalContIO do
    when (VS.length table /= 256) do
        let msg = "Length of color value lookup table is not 256."
        liftIO $ Control.Exception.throwIO $ BadArgumentError msg

    -- NOTE: Skia copies the table content
    table' <- useStorableVector table
    result' <- liftIO $ sk_colorfilter_new_table table'
    toObjectFin sk_colorfilter_unref result'

-- | Throws an error if size of any lookup table is not 256.
createTableARGB ::
    (MonadIO m) =>
    -- | Alpha value lookup table of type (Word8 -> Word8) for alpha encoded as
    -- a 256-length vector.
    VS.Vector Word8 ->
    -- | Alpha value lookup table of type (Word8 -> Word8) for red encoded as
    -- a 256-length vector.
    VS.Vector Word8 ->
    -- | Alpha value lookup table of type (Word8 -> Word8) for green encoded as
    -- a 256-length vector.
    VS.Vector Word8 ->
    -- | Alpha value lookup table of type (Word8 -> Word8) for blue encoded as
    -- a 256-length vector.
    VS.Vector Word8 ->
    m SKColorFilter
createTableARGB tableA tableR tableG tableB = evalContIO do
    let
        checkSizeAndUse name table = do
            when (VS.length table /= 256) do
                let msg = "Length of " <> name <> " value lookup table is not 256."
                liftIO $ Control.Exception.throwIO $ BadArgumentError msg
            useStorableVector table

    tableA' <- checkSizeAndUse "alpha" tableA
    tableR' <- checkSizeAndUse "red" tableR
    tableG' <- checkSizeAndUse "green" tableG
    tableB' <- checkSizeAndUse "blue" tableB

    result' <- liftIO $ sk_colorfilter_new_table_argb tableA' tableR' tableG' tableB'
    toObjectFin sk_colorfilter_unref result'
