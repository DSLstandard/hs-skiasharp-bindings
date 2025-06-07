module Skia.SKMaskFilter where

import Control.Exception qualified
import Data.Vector.Storable qualified as VS
import Skia.Internal.Prelude

createTable ::
    (MonadIO m) =>
    -- | Gamma lookup table of type (Word8 -> Word8) encoded as a
    -- 256-length vector.
    VS.Vector Word8 ->
    m SKMaskFilter
createTable table = evalContIO do
    when (VS.length table /= 256) do
        let msg = "Length of gamma table is not 256."
        liftIO $ Control.Exception.throwIO $ BadArgumentError msg

    table' <- useStorableVector table
    result' <- liftIO $ sk_maskfilter_new_table table'
    toObjectFin sk_maskfilter_unref result'

createGamma ::
    (MonadIO m) =>
    -- | Gamma
    Float ->
    m SKMaskFilter
createGamma gamma = liftIO do
    result' <- liftIO $ sk_maskfilter_new_gamma (coerce gamma)
    toObjectFin sk_maskfilter_unref result'

createClip ::
    (MonadIO m) =>
    -- | (min, max)
    (Word8, Word8) ->
    m SKMaskFilter
createClip (minv, maxv) = liftIO do
    result' <- sk_maskfilter_new_clip (coerce minv) (coerce maxv)
    toObjectFin sk_maskfilter_unref result'

createBlur ::
    (MonadIO m) =>
    SKBlurStyle ->
    -- | \"sigma|". Standard deviation of the Gaussian blur to apply. Must be > 0.
    Float ->
    -- | \"respectCTM\". If true, the blur's sigma is modified by the CTM.
    --
    -- For reference, the default value of this parameter is @true@ in Google Skia.
    Bool ->
    m SKMaskFilter
createBlur style sigma respectCTM = liftIO do
    result' <- sk_maskfilter_new_blur_with_flags (marshalSKEnum style) (coerce sigma) (fromBool respectCTM)
    toObjectFin sk_maskfilter_unref result'

createShader :: (MonadIO m) => SKShader -> m SKMaskFilter
createShader shader = evalContIO do
    shader' <- useObj shader
    result' <- liftIO $ sk_maskfilter_new_shader shader'
    toObjectFin sk_maskfilter_unref result'
