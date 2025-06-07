module Skia.GRRecordingContext where

import Skia.Internal.Prelude

asDirectContextRaw :: (MonadIO m) => Ptr Gr_recording_context -> m (Maybe (Ptr Gr_direct_context))
asDirectContextRaw recordingctx = liftIO do
    directctx <- gr_recording_context_get_direct_context recordingctx
    if directctx == nullPtr
        then pure Nothing
        else pure (Just directctx)

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
