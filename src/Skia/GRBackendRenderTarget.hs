module Skia.GRBackendRenderTarget where

import Skia.Internal.Prelude

destroy :: (MonadIO m) => GRBackendRenderTarget -> m ()
destroy tgt = evalContIO do
    tgt' <- useObj tgt
    liftIO $ gr_backendrendertarget_delete tgt'

isValid :: (MonadIO m) => GRBackendRenderTarget -> m Bool
isValid tgt = evalContIO do
    tgt' <- useObj tgt
    liftIO $ fmap toBool $ gr_backendrendertarget_is_valid tgt'

createGl ::
    (MonadIO m) =>
    -- | Width
    Int ->
    -- | Height
    Int ->
    -- | Sample count
    Int ->
    -- | Stencil bits
    Int ->
    Gr_gl_framebufferinfo ->
    m (Owned GRBackendRenderTarget)
createGl width height sampleCount stencilBits glInfo = evalContIO do
    glInfo' <- useStorable glInfo
    liftIO $
        toObject
            =<< gr_backendrendertarget_new_gl
                (fromIntegral width)
                (fromIntegral height)
                (fromIntegral sampleCount)
                (fromIntegral stencilBits)
                glInfo'

createVulkan ::
    (MonadIO m) =>
    -- | Width
    Int ->
    -- | Height
    Int ->
    Gr_vk_imageinfo ->
    m GRBackendRenderTarget
createVulkan width height vkImageInfo = evalContIO do
    vkImageInfo' <- useStorable vkImageInfo
    liftIO $
        toObject
            =<< gr_backendrendertarget_new_vulkan
                (fromIntegral width)
                (fromIntegral height)
                vkImageInfo'

createMetal ::
    (MonadIO m) =>
    -- | Width
    Int ->
    -- | Height
    Int ->
    Gr_mtl_textureinfo ->
    m GRBackendRenderTarget
createMetal width height textureInfo = evalContIO do
    textureInfo' <- useStorable textureInfo
    liftIO $
        toObject
            =<< gr_backendrendertarget_new_metal
                (fromIntegral width)
                (fromIntegral height)
                textureInfo'

createDirect3D ::
    (MonadIO m) =>
    -- | Width
    Int ->
    -- | Height
    Int ->
    Gr_d3d_textureresourceinfo ->
    m GRBackendRenderTarget
createDirect3D width height textureInfo = evalContIO do
    textureInfo' <- useStorable textureInfo
    liftIO $
        toObject
            =<< gr_backendrendertarget_new_direct3d
                (fromIntegral width)
                (fromIntegral height)
                textureInfo'

getWidth :: (MonadIO m) => GRBackendRenderTarget -> m Int
getWidth tgt = evalContIO do
    tgt' <- useObj tgt
    liftIO $ fmap fromIntegral $ gr_backendrendertarget_get_width tgt'

getHeight :: (MonadIO m) => GRBackendRenderTarget -> m Int
getHeight tgt = evalContIO do
    tgt' <- useObj tgt
    liftIO $ fmap fromIntegral $ gr_backendrendertarget_get_height tgt'

getSampleCount :: (MonadIO m) => GRBackendRenderTarget -> m Int
getSampleCount tgt = evalContIO do
    tgt' <- useObj tgt
    liftIO $ fmap fromIntegral $ gr_backendrendertarget_get_samples tgt'

getStencilBits :: (MonadIO m) => GRBackendRenderTarget -> m Int
getStencilBits tgt = evalContIO do
    tgt' <- useObj tgt
    liftIO $ fmap fromIntegral $ gr_backendrendertarget_get_stencils tgt'

getBackend :: (MonadIO m) => GRBackendRenderTarget -> m GRBackend
getBackend tgt = evalContIO do
    tgt' <- useObj tgt
    liftIO $ unmarshalSKEnumOrDie =<< gr_backendrendertarget_get_backend tgt'

-- | Returns 'Nothing' if the target does not have 'GlFramebufferInfo'.
getGlFramebufferInfo :: (MonadIO m) => GRBackendRenderTarget -> m (Maybe Gr_gl_framebufferinfo)
getGlFramebufferInfo tgt = evalContIO do
    glInfo' <- useAlloca
    tgt' <- useObj tgt
    success <- liftIO $ fmap toBool $ gr_backendrendertarget_get_gl_framebufferinfo tgt' glInfo'
    if success
        then do
            glInfo <- liftIO $ peek glInfo'
            pure $ Just glInfo
        else do
            pure Nothing
