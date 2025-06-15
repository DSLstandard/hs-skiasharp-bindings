module Skia.GRBackendTexture (
    -- * Creating 'GRBackendTexture'
    createGl,
    createVulkan,
    createMetal,
    createDirect3D,

    -- * Getters
    isValid,
    getWidth,
    getHeight,
    hasMipmaps,
    getBackend,
    getGlTextureInfo,
)
where

import Skia.Internal.Prelude

{- | Creates a new OpenGL 'GRBackendTexture' with the specified properties and
texture.

The 'GRBackendTexture' is destroyed when 'Acquire' releases.
-}
createGl ::
    -- | Width
    Int ->
    -- | Height
    Int ->
    -- | Is the texture mipmapped?
    Bool ->
    Gr_gl_textureinfo ->
    Acquire GRBackendTexture
createGl width height mipmapped glInfo =
    mkSKObjectAcquire
        ( evalContIO do
            glInfo' <- useStorable glInfo
            liftIO $
                gr_backendtexture_new_gl
                    (fromIntegral width)
                    (fromIntegral height)
                    (fromBool mipmapped)
                    glInfo'
        )
        gr_backendtexture_delete

{- | Creates a new Vulkan 'GRBackendTexture' with the specified properties and
texture.

The 'GRBackendTexture' is destroyed when 'Acquire' releases.
-}
createVulkan ::
    -- | Width
    Int ->
    -- | Height
    Int ->
    Gr_vk_imageinfo ->
    Acquire GRBackendTexture
createVulkan width height vkImageInfo =
    mkSKObjectAcquire
        ( evalContIO do
            vkImageInfo' <- useStorable vkImageInfo
            liftIO $
                gr_backendtexture_new_vulkan
                    (fromIntegral width)
                    (fromIntegral height)
                    vkImageInfo'
        )
        gr_backendtexture_delete

{- | Creates a new Metal 'GRBackendTexture' with the specified properties and
texture.

The 'GRBackendTexture' is destroyed when 'Acquire' releases.
-}
createMetal ::
    -- | Width
    Int ->
    -- | Height
    Int ->
    -- | Mipmapped?
    Bool ->
    Gr_mtl_textureinfo ->
    Acquire GRBackendTexture
createMetal width height mipmapped textureInfo =
    mkSKObjectAcquire
        ( evalContIO do
            textureInfo' <- useStorable textureInfo
            liftIO $
                gr_backendtexture_new_metal
                    (fromIntegral width)
                    (fromIntegral height)
                    (fromBool mipmapped)
                    textureInfo'
        )
        gr_backendtexture_delete

{- | Creates a new Direct3D 'GRBackendTexture' with the specified properties and
texture.

The 'GRBackendTexture' is destroyed when 'Acquire' releases.
-}
createDirect3D ::
    -- | Width
    Int ->
    -- | Height
    Int ->
    Gr_d3d_textureresourceinfo ->
    Acquire GRBackendTexture
createDirect3D width height textureInfo =
    mkSKObjectAcquire
        ( evalContIO do
            textureInfo' <- useStorable textureInfo
            liftIO $
                gr_backendtexture_new_direct3d
                    (fromIntegral width)
                    (fromIntegral height)
                    textureInfo'
        )
        gr_backendtexture_delete

-- | Gets the width in pixels.
getWidth :: (MonadIO m) => GRBackendTexture -> m Int
getWidth tex = evalContIO do
    liftIO $ fmap fromIntegral $ gr_backendtexture_get_width (ptr tex)

-- | Gets the height in pixels.
getHeight :: (MonadIO m) => GRBackendTexture -> m Int
getHeight tex = evalContIO do
    liftIO $ fmap fromIntegral $ gr_backendtexture_get_height (ptr tex)

-- | Returns true if this texture is mipmapped.
hasMipmaps :: (MonadIO m) => GRBackendTexture -> m Bool
hasMipmaps tex = evalContIO do
    liftIO $ fmap toBool $ gr_backendtexture_has_mipmaps (ptr tex)

-- | Gets the backend type for this render target.
getBackend :: (MonadIO m) => GRBackendTexture -> m GRBackend
getBackend tex = evalContIO do
    liftIO $ unmarshalSKEnumOrDie =<< gr_backendtexture_get_backend (ptr tex)

{- | If the backend API is GL, copies a snapshot of the 'Gr_gl_textureinfo'
struct into the passed in pointer and returns true. Otherwise returns 'Nothing'
if the backend API is not GL.
-}
getGlTextureInfo :: (MonadIO m) => GRBackendTexture -> m (Maybe Gr_gl_textureinfo)
getGlTextureInfo tex = evalContIO do
    glInfo' <- useAlloca

    success <- liftIO $ fmap toBool $ gr_backendtexture_get_gl_textureinfo (ptr tex) glInfo'

    if success
        then do
            glInfo <- liftIO $ peek glInfo'
            pure $ Just glInfo
        else do
            pure Nothing

-- | Returns true if the backend texture has been initialized.
isValid :: (MonadIO m) => GRBackendTexture -> m Bool
isValid tex = evalContIO do
    liftIO $ fmap toBool $ gr_backendtexture_is_valid (ptr tex)
