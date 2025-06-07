module Skia.SKPictureRecorder where

import Skia.Internal.Prelude

destroy :: (MonadIO m) => SKPictureRecorder -> m ()
destroy picrec = evalContIO do
    picrec' <- useObj picrec
    liftIO $ sk_picture_recorder_delete picrec'

create :: (MonadIO m) => m SKPictureRecorder
create = liftIO do
    picrec' <- sk_picture_recorder_new
    toObject picrec'

-- | Returns the canvas that records the drawing commands.
beginRecording ::
    (MonadIO m, IsSKBBHFactory factory) =>
    SKPictureRecorder ->
    -- | Bounds
    Rect Float ->
    -- | Optional acceleration structure for display optimization for culling
    -- invisible calls recorded.
    --
    -- You must manually delete the structure when it is no longer used.
    --
    -- The only option in this library is 'SKRTreeFactory'. You can use the
    -- functions 'createRTreeFactory' and 'destroyRTreeFactory' to create and
    -- destroy 'SKRTreeFactory' respectively.
    Maybe factory ->
    m SKCanvas
beginRecording picrec bounds bbhFactory = evalContIO do
    picrec' <- useObj picrec
    bounds' <- useStorable $ toSKRect bounds

    canvas' <- case bbhFactory of
        Nothing -> do
            liftIO $ sk_picture_recorder_begin_recording picrec' bounds'
        Just bbhFactory -> do
            bbhFactory' <- useObj (bbhFactory `asA` SKBBHFactory)
            liftIO $ sk_picture_recorder_begin_recording_with_bbh_factory picrec' bounds' bbhFactory'

    toObject canvas'

finishRecordingAsPicture :: (MonadIO m) => SKPictureRecorder -> m SKPicture
finishRecordingAsPicture picrec = evalContIO do
    picrec' <- useObj picrec
    picture' <- liftIO $ sk_picture_recorder_end_recording picrec'
    toObjectFin sk_picture_unref picture'

finishRecordingAsDrawable :: (MonadIO m) => SKPictureRecorder -> m SKDrawable
finishRecordingAsDrawable picrec = evalContIO do
    picrec' <- useObj picrec
    drawable' <- liftIO $ sk_picture_recorder_end_recording_as_drawable picrec'
    toObjectFin sk_drawable_unref drawable'

{- | Returns the recording canvas if one is active, or 'Nothing' if recording is
not active. This does not alter the refcnt on the canvas (if present).
-}
getRecordingCanvas :: (MonadIO m) => SKPictureRecorder -> m (Maybe SKCanvas)
getRecordingCanvas picrec = evalContIO do
    picrec' <- useObj picrec
    canvas' <- liftIO $ sk_picture_get_recording_canvas picrec'
    if canvas' == nullPtr
        then pure Nothing
        else Just <$> toObject canvas'

createRTreeFactory :: (MonadIO m) => m SKRTreeFactory
createRTreeFactory = liftIO do
    toObject =<< sk_rtree_factory_new

destroyRTreeFactory :: (MonadIO m) => SKRTreeFactory -> m ()
destroyRTreeFactory rtree = evalContIO do
    rtree' <- useObj rtree
    liftIO $ sk_rtree_factory_delete rtree'
