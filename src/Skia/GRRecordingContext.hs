module Skia.GRRecordingContext where

import Skia.Internal.Prelude

{- | Upcast the input 'GRRecordingContext' to a 'GRDirectContext' if the
recording context is actually a direct context, which offers extra
functionalities.
-}
asDirectContext :: (MonadIO m) => GRRecordingContext -> m (Maybe GRDirectContext)
asDirectContext recordingctx = evalContIO do
    -- TODO: Is this implementation correct? Could directctx' actually a new
    -- instance of 'GRDirectContext'? I could not find helpful Google Skia
    -- documentation about this.
    recordingctx' <- useObj recordingctx
    directctx' <- liftIO $ gr_recording_context_get_direct_context recordingctx'
    if directctx' == nullPtr
        then do
            pure Nothing
        else do
            -- We directly cast the object instead of doing 'toObject' so the
            -- returned 'GRDirectContext' has the same 'ForeignPtr' as the input
            -- 'GRRecordingContext' and maintain the finalizer of the
            -- 'ForeignPtr'.
            pure $ Just (unsafeCastObject recordingctx)

getBackend :: (MonadIO m, IsGRRecordingContext context) => context -> m GRBackend
getBackend (toA GRRecordingContext -> ctx) = evalContIO do
    ctx' <- useObj ctx
    liftIO $ unmarshalSKEnumOrDie =<< gr_recording_context_get_backend ctx'

isAbandoned :: (MonadIO m, IsGRRecordingContext context) => context -> m Bool
isAbandoned (toA GRRecordingContext -> ctx) = evalContIO do
    ctx' <- useObj ctx
    liftIO $ fmap toBool $ gr_recording_context_is_abandoned ctx'

getMaxTextureSize :: (MonadIO m, IsGRRecordingContext context) => context -> m Int
getMaxTextureSize (toA GRRecordingContext -> ctx) = evalContIO do
    ctx' <- useObj ctx
    liftIO $ fmap fromIntegral $ gr_recording_context_max_texture_size ctx'

getMaxRenderTargetSize :: (MonadIO m, IsGRRecordingContext context) => context -> m Int
getMaxRenderTargetSize (toA GRRecordingContext -> ctx) = evalContIO do
    ctx' <- useObj ctx
    liftIO $ fmap fromIntegral $ gr_recording_context_max_render_target_size ctx'

getMaxSurfaceSampleCount :: (MonadIO m, IsGRRecordingContext context) => context -> SKColorType -> m Int
getMaxSurfaceSampleCount (toA GRRecordingContext -> ctx) colorType = evalContIO do
    ctx' <- useObj ctx
    liftIO $ fmap fromIntegral $ gr_recording_context_get_max_surface_sample_count_for_color_type ctx' (marshalSKEnum colorType)
