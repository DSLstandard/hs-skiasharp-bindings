module Skia.SKDrawable where

import Linear
import Skia.Internal.Prelude

newtype GenerationId = GenerationId {unGenerationId :: Word32}
    deriving (Show, Eq, Ord)
    deriving newtype (Num)

getGenerationId :: (MonadIO m, IsSKDrawable drawable) => drawable -> m GenerationId
getGenerationId (toA SKDrawable -> drawable) = evalContIO do
    drawable' <- useObj drawable
    liftIO $ fmap GenerationId $ sk_drawable_get_generation_id drawable'

getBounds :: (MonadIO m, IsSKDrawable drawable) => drawable -> m (Rect Float)
getBounds (toA SKDrawable -> drawable) = evalContIO do
    drawable' <- useObj drawable
    rect' <- useAlloca
    liftIO $ sk_drawable_get_bounds drawable' rect'
    peekWith fromSKRect rect'

draw ::
    (MonadIO m, IsSKDrawable drawable, IsSKCanvas canvas) =>
    drawable ->
    -- | Destination canvas
    canvas ->
    -- | Optional transform
    Maybe (M33 Float) ->
    m ()
draw (toA SKDrawable -> drawable) (toA SKCanvas -> canvas) matrix = evalContIO do
    drawable' <- useObj drawable
    canvas' <- useObj canvas
    matrix' <- useNullIfNothing useStorable (toSKMatrix <$> matrix)
    liftIO $ sk_drawable_draw drawable' canvas' matrix'

makePictureSnapshot :: (IsSKDrawable drawable) => drawable -> Acquire SKPicture
makePictureSnapshot (toA SKDrawable -> drawable) =
    mkSKObjectAcquire
        (sk_drawable_new_picture_snapshot (ptr drawable))
        sk_picture_unref

notifyDrawingChanged :: (MonadIO m, IsSKDrawable drawable) => drawable -> m ()
notifyDrawingChanged (toA SKDrawable -> drawable) = evalContIO do
    drawable' <- useObj drawable
    liftIO $ sk_drawable_notify_drawing_changed drawable'

getApproximateBytesUsed :: (MonadIO m, IsSKDrawable drawable) => drawable -> m Int
getApproximateBytesUsed (toA SKDrawable -> drawable) = evalContIO do
    drawable' <- useObj drawable
    liftIO $ fmap fromIntegral $ sk_drawable_approximate_bytes_used drawable'
