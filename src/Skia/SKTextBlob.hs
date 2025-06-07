{-# LANGUAGE CPP #-}

module Skia.SKTextBlob where

import Control.Exception qualified
import Data.Vector qualified as Vector
import Skia.Internal.Prelude

{- | Returns conservative bounding box. Uses 'SKPaint' associated with each
glyph to determine glyph bounds, and unions all bounds. Returned bounds may be
larger than the bounds of all glyphs in runs.
-}
getBounds ::
    (MonadIO m) =>
    SKTextBlob ->
    -- | conservative bounding box
    m (Rect Float)
getBounds blob = evalContIO do
    blob' <- useObj blob
    bounds' <- useAlloca
    liftIO $ sk_textblob_get_bounds blob' bounds'
    peekWith fromSKRect bounds'

-- | Returns a non-zero value unique among all text blobs.
getUniqueId ::
    (MonadIO m) =>
    SKTextBlob ->
    -- | identifier for 'SKTextBlob'
    m Word32
getUniqueId blob = evalContIO do
    blob' <- useObj blob
    liftIO $ sk_textblob_get_unique_id blob'

{- | Returns a list of intervals that intersect \"bounds\". \"bounds\" describes
a pair of lines parallel to the text advance.

Runs within the blob that contain SkRSXform are ignored when computing
intercepts.
-}
getIntercepts ::
    (MonadIO m) =>
    SKTextBlob ->
    -- | \"bounds\". Lower and upper line parallel to the advance.
    (Float, Float) ->
    -- | \"paint\". Optional, specifies stroking, 'SKPathEffect' that affects
    -- the result.
    Maybe SKPaint ->
    m (Vector.Vector (Float, Float))
getIntercepts blob (bounds'lower, bounds'upper) paint = evalContIO do
    blob' <- useObj blob
    paint' <- useNullIfNothing useObj paint
    bounds' <- ContT $ withArray $ coerce [bounds'lower, bounds'upper]

    -- NOTE: Google Skia's comment: Pass nullptr for intervals to determine the
    -- size of the interval array.
    count :: Int <- liftIO $ fmap fromIntegral $ sk_textblob_get_intercepts blob' bounds' nullPtr paint'

    -- NOTE: Google Skia's comment: The return count is zero or a multiple of
    -- two, and is at most twice the number of glyphs in the the blob.
#ifdef HS_SKIA_SKIA_ASSERTIONS_ENABLED
    when (not $ even count) do
        let msg = "sk_textblob_get_intercepts should always return an even count, but got " <> show count
        liftIO $ Control.Exception.throwIO $ SkiaAssertionError msg
#endif

    intervals' <- ContT $ allocaArray count

#ifdef HS_SKIA_SKIA_ASSERTIONS_ENABLED
    countNew <- liftIO $ fmap fromIntegral $ sk_textblob_get_intercepts blob' bounds' intervals' paint'
    when (count /= countNew) do
        let msg = "sk_textblob_get_intercepts should return the same 'count' in the second call, but got " <> show countNew
        liftIO $ Control.Exception.throwIO $ SkiaAssertionError msg
#else
    void $ liftIO $ fmap fromIntegral $ sk_textblob_get_intercepts blob' bounds' intervals' paint'
#endif

    Vector.generateM count \i -> do
        start <- liftIO $ peekElemOff intervals' (2*i)
        end <- liftIO $ peekElemOff intervals' (2*i+1)
        pure (coerce start, coerce end)


