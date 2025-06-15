module Skia.SKColorFilter where

import Control.Exception
import Data.Vector.Storable qualified as VS
import Skia.Internal.Prelude
import Skia.Types.ColorMatrix qualified as ColorMatrix

createBlend :: SKColor -> SKBlendMode -> Acquire SKColorFilter
createBlend color blendMode =
    mkSKObjectAcquire
        (sk_colorfilter_new_mode (coerce color) (marshalSKEnum blendMode))
        sk_colorfilter_unref

{- | Create a colorfilter that multiplies the RGB channels by one color, and
then adds a second color, pinning the result for each component to [0..255].
The alpha components of the mul and add arguments are ignored.
-}
createLighting ::
    -- | \"mul\"
    SKColor ->
    -- | \"add\"
    SKColor ->
    Acquire SKColorFilter
createLighting mul add =
    mkSKObjectAcquire
        (sk_colorfilter_new_lighting (coerce mul) (coerce add))
        sk_colorfilter_unref

createCompose ::
    -- | Outer
    SKColorFilter ->
    -- | Inner
    SKColorFilter ->
    Acquire SKColorFilter
createCompose outer inner =
    mkSKObjectAcquire
        (sk_colorfilter_new_compose (ptr outer) (ptr inner))
        sk_colorfilter_unref

-- | Raises 'BadArgumentError' if the input matrix contains infinity.
createColorMatrix ::
    ColorMatrix Float ->
    Acquire SKColorFilter
createColorMatrix matrix =
    mkSKObjectAcquire
        ( do
            cf' <- ColorMatrix.unsafeWith matrix \array' -> do
                sk_colorfilter_new_color_matrix (castPtr array')

            -- See src/effects/colorfilters/SkMatrixColorFilter.cpp. The returned
            -- color filter could be nullptr if the input matrix contains infinity.
            when (cf' == nullPtr) do
                throwIO $ BadArgumentError "input matrix must not contain infinity values"

            pure cf'
        )
        sk_colorfilter_unref

createHslaMatrix :: ColorMatrix Float -> Acquire SKColorFilter
createHslaMatrix matrix =
    mkSKObjectAcquire
        ( do
            cf' <- ColorMatrix.unsafeWith matrix \array' -> do
                sk_colorfilter_new_hsla_matrix (castPtr array')

            -- See src/effects/colorfilters/SkMatrixColorFilter.cpp. The returned
            -- color filter could be nullptr if the input matrix contains infinity.
            when (cf' == nullPtr) do
                throwIO $ BadArgumentError "input matrix must not contain infinity values"

            pure cf'
        )
        sk_colorfilter_unref

createLinearToSrgbGamma :: Acquire SKColorFilter
createLinearToSrgbGamma =
    mkSKObjectAcquire
        (sk_colorfilter_new_linear_to_srgb_gamma)
        sk_colorfilter_unref

createSrgbToLinearGamma :: Acquire SKColorFilter
createSrgbToLinearGamma =
    mkSKObjectAcquire
        sk_colorfilter_new_srgb_to_linear_gamma
        sk_colorfilter_unref

-- TODO: t = 0 => dest/src? Need to confirm.
createLerp ::
    -- | t
    Float ->
    -- | Destination
    SKColorFilter ->
    -- | Source
    SKColorFilter ->
    Acquire SKColorFilter
createLerp t dst src =
    mkSKObjectAcquire
        (sk_colorfilter_new_lerp (coerce t) (ptr dst) (ptr src))
        sk_colorfilter_unref

createLumaColor :: Acquire SKColorFilter
createLumaColor =
    mkSKObjectAcquire
        sk_colorfilter_new_luma_color
        sk_colorfilter_unref

createHighContrast ::
    -- | Grayscale?
    Bool ->
    SKHighContrastConfigInvertStyle ->
    -- | Contrast
    Float ->
    Acquire SKColorFilter
createHighContrast grayscale invertStyle contrast =
    mkSKObjectAcquire
        ( evalContIO do
            conf' <-
                useStorable
                    Sk_highcontrastconfig
                        { fGrayscale = fromBool grayscale
                        , fInvertStyle = marshalSKEnum invertStyle
                        , fContrast = coerce contrast
                        }
            liftIO $ sk_colorfilter_new_high_contrast conf'
        )
        sk_colorfilter_unref

-- | Throws a 'BadArgumentError' if size of the lookup table is not 256.
createTable ::
    -- | Color value lookup table of type (Word8 -> Word8) encoded as a
    -- 256-length vector.
    VS.Vector Word8 ->
    Acquire SKColorFilter
createTable table =
    mkSKObjectAcquire
        ( evalContIO do
            when (VS.length table /= 256) do
                let msg = "Length of color value lookup table is not 256."
                liftIO $ Control.Exception.throwIO $ BadArgumentError msg

            -- NOTE: Skia copies the table content
            table' <- useStorableVector table
            liftIO $ sk_colorfilter_new_table table'
        )
        sk_colorfilter_unref

-- | Throws an error if size of any lookup table is not 256.
createTableARGB ::
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
    Acquire SKColorFilter
createTableARGB tableA tableR tableG tableB =
    mkSKObjectAcquire
        ( evalContIO do
            let checkSizeAndUse name table = do
                    when (VS.length table /= 256) do
                        let msg = "Length of " <> name <> " value lookup table is not 256."
                        liftIO $ Control.Exception.throwIO $ BadArgumentError msg
                    useStorableVector table

            tableA' <- checkSizeAndUse "alpha" tableA
            tableR' <- checkSizeAndUse "red" tableR
            tableG' <- checkSizeAndUse "green" tableG
            tableB' <- checkSizeAndUse "blue" tableB

            liftIO $ sk_colorfilter_new_table_argb tableA' tableR' tableG' tableB'
        )
        sk_colorfilter_unref
