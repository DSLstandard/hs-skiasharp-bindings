module Skia.GRBackendRenderTarget where

import Skia.Internal.Prelude

-- * Creating 'GRBackendRenderTarget'

-- NOTE: 'createGl' and friends have no documentation in Skia, so we are taking
-- documentations from
-- https://learn.microsoft.com/en-us/dotnet/api/skiasharp.grbackendrendertarget?view=skiasharp-2.88.
-- instead.

{- | Creates a new OpenGL 'GRBackendRenderTarget' with the specified properties
and framebuffer.

The 'GRBackendRenderTarget' is destroyed when 'Acquire' releases.
-}
createGl ::
    -- | Width
    Int ->
    -- | Height
    Int ->
    -- | Sample count
    Int ->
    -- | Stencil bits
    Int ->
    Gr_gl_framebufferinfo ->
    Acquire GRBackendRenderTarget
createGl width height sampleCount stencilBits glInfo =
    mkSKObjectAcquire
        ( evalContIO do
            glInfo' <- useStorable glInfo
            liftIO $
                gr_backendrendertarget_new_gl
                    (fromIntegral width)
                    (fromIntegral height)
                    (fromIntegral sampleCount)
                    (fromIntegral stencilBits)
                    glInfo'
        )
        gr_backendrendertarget_delete

{- | Creates a new Vulkan 'GRBackendRenderTarget' with the specified properties
and framebuffer.

The 'GRBackendRenderTarget' is destroyed when 'Acquire' releases.
-}
createVulkan ::
    -- | Width
    Int ->
    -- | Height
    Int ->
    Gr_vk_imageinfo ->
    Acquire GRBackendRenderTarget
createVulkan width height vkImageInfo =
    mkSKObjectAcquire
        ( evalContIO do
            vkImageInfo' <- useStorable vkImageInfo
            liftIO $
                gr_backendrendertarget_new_vulkan
                    (fromIntegral width)
                    (fromIntegral height)
                    vkImageInfo'
        )
        gr_backendrendertarget_delete

{- | Creates a new Metal 'GRBackendRenderTarget' with the specified properties
and framebuffer.

The 'GRBackendRenderTarget' is destroyed when 'Acquire' releases.
-}
createMetal ::
    -- | Width
    Int ->
    -- | Height
    Int ->
    Gr_mtl_textureinfo ->
    Acquire GRBackendRenderTarget
createMetal width height textureInfo =
    mkSKObjectAcquire
        ( evalContIO do
            textureInfo' <- useStorable textureInfo
            liftIO $
                gr_backendrendertarget_new_metal
                    (fromIntegral width)
                    (fromIntegral height)
                    textureInfo'
        )
        gr_backendrendertarget_delete

{- | Creates a new Direct3D 'GRBackendRenderTarget' with the specified properties
and framebuffer.

The 'GRBackendRenderTarget' is destroyed when 'Acquire' releases.
-}
createDirect3D ::
    -- | Width
    Int ->
    -- | Height
    Int ->
    Gr_d3d_textureresourceinfo ->
    Acquire GRBackendRenderTarget
createDirect3D width height textureInfo =
    mkSKObjectAcquire
        ( evalContIO do
            textureInfo' <- useStorable textureInfo
            liftIO $
                gr_backendrendertarget_new_direct3d
                    (fromIntegral width)
                    (fromIntegral height)
                    textureInfo'
        )
        gr_backendrendertarget_delete

-- * Getters

-- | Returns true if the 'GRBackendRenderTarget' has been initialized.
isValid :: (MonadIO m) => GRBackendRenderTarget -> m Bool
isValid tgt = do
    liftIO $ fmap toBool $ gr_backendrendertarget_is_valid (ptr tgt)

-- | Gets the width in pixels.
getWidth :: (MonadIO m) => GRBackendRenderTarget -> m Int
getWidth tgt = liftIO do
    fmap fromIntegral $ gr_backendrendertarget_get_width (ptr tgt)

-- | Gets the height in pixels.
getHeight :: (MonadIO m) => GRBackendRenderTarget -> m Int
getHeight tgt = liftIO do
    fmap fromIntegral $ gr_backendrendertarget_get_height (ptr tgt)

-- | Gets the number of samples per pixel.
getSampleCount :: (MonadIO m) => GRBackendRenderTarget -> m Int
getSampleCount tgt = liftIO do
    fmap fromIntegral $ gr_backendrendertarget_get_samples (ptr tgt)

-- | Gets the number of bits of stencil per-pixel.
getStencilBits :: (MonadIO m) => GRBackendRenderTarget -> m Int
getStencilBits tgt = liftIO do
    fmap fromIntegral $ gr_backendrendertarget_get_stencils (ptr tgt)

-- | Gets the backend type for this render target.
getBackend :: (MonadIO m) => GRBackendRenderTarget -> m GRBackend
getBackend tgt = liftIO do
    unmarshalSKEnumOrDie =<< gr_backendrendertarget_get_backend (ptr tgt)

-- | Returns 'Nothing' if the target does not have 'GlFramebufferInfo'.
getGlFramebufferInfo :: (MonadIO m) => GRBackendRenderTarget -> m (Maybe Gr_gl_framebufferinfo)
getGlFramebufferInfo tgt = evalContIO do
    glInfo' <- useAlloca
    success <- liftIO $ fmap toBool $ gr_backendrendertarget_get_gl_framebufferinfo (ptr tgt) glInfo'
    if success
        then do
            glInfo <- liftIO $ peek glInfo'
            pure $ Just glInfo
        else do
            pure Nothing
