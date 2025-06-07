module Skia.GRBackendTexture (
    delete,
    isValid,
    createGl,
    createVulkan,
    createMetal,
    createDirect3D,
    getWidth,
    getHeight,
    hasMipmaps,
    getBackend,
    getGlTextureInfo,
)
where

import Skia.Internal.Prelude

delete :: (MonadIO m) => GRBackendTexture -> m ()
delete tex = evalContIO do
    tex' <- useObj tex
    liftIO $ gr_backendtexture_delete tex'

isValid :: (MonadIO m) => GRBackendTexture -> m Bool
isValid tex = evalContIO do
    tex' <- useObj tex
    liftIO $ fmap toBool $ gr_backendtexture_is_valid tex'

-- | Returns 'Nothing' if failed.
createGl ::
    (MonadIO m) =>
    -- | Width
    Int ->
    -- | Height
    Int ->
    -- | Mipmapped?
    Bool ->
    Gr_gl_textureinfo ->
    m GRBackendTexture
createGl width height mipmapped glInfo = evalContIO do
    glInfo' <- useStorable glInfo
    liftIO $
        toObject
            =<< gr_backendtexture_new_gl
                (fromIntegral width)
                (fromIntegral height)
                (fromBool mipmapped)
                glInfo'

createVulkan ::
    (MonadIO m) =>
    -- | Width
    Int ->
    -- | Height
    Int ->
    Gr_vk_imageinfo ->
    m GRBackendTexture
createVulkan width height vkImageInfo = evalContIO do
    vkImageInfo' <- useStorable vkImageInfo
    liftIO $
        toObject
            =<< gr_backendtexture_new_vulkan
                (fromIntegral width)
                (fromIntegral height)
                vkImageInfo'

createMetal ::
    (MonadIO m) =>
    -- | Width
    Int ->
    -- | Height
    Int ->
    -- | Mipmapped?
    Bool ->
    Gr_mtl_textureinfo ->
    m GRBackendTexture
createMetal width height mipmapped textureInfo = evalContIO do
    textureInfo' <- useStorable textureInfo
    liftIO $
        toObject
            =<< gr_backendtexture_new_metal
                (fromIntegral width)
                (fromIntegral height)
                (fromBool mipmapped)
                textureInfo'

createDirect3D ::
    (MonadIO m) =>
    -- | Width
    Int ->
    -- | Height
    Int ->
    Gr_d3d_textureresourceinfo ->
    m GRBackendTexture
createDirect3D width height textureInfo = evalContIO do
    textureInfo' <- useStorable textureInfo
    liftIO $
        toObject
            =<< gr_backendtexture_new_direct3d
                (fromIntegral width)
                (fromIntegral height)
                textureInfo'

getWidth :: (MonadIO m) => GRBackendTexture -> m Int
getWidth tex = evalContIO do
    tex' <- useObj tex
    liftIO $ fmap fromIntegral $ gr_backendtexture_get_width tex'

getHeight :: (MonadIO m) => GRBackendTexture -> m Int
getHeight tex = evalContIO do
    tex' <- useObj tex
    liftIO $ fmap fromIntegral $ gr_backendtexture_get_height tex'

hasMipmaps :: (MonadIO m) => GRBackendTexture -> m Bool
hasMipmaps tex = evalContIO do
    tex' <- useObj tex
    liftIO $ fmap toBool $ gr_backendtexture_has_mipmaps tex'

getBackend :: (MonadIO m) => GRBackendTexture -> m GRBackend
getBackend tex = evalContIO do
    tex' <- useObj tex
    liftIO $ unmarshalSKEnumOrDie =<< gr_backendtexture_get_backend tex'

{- | If the backend API is GL, copies a snapshot of the 'Gr_gl_textureinfo'
struct into the passed in pointer and returns true. Otherwise returns 'Nothing'
if the backend API is not GL.
-}
getGlTextureInfo :: (MonadIO m) => GRBackendTexture -> m (Maybe Gr_gl_textureinfo)
getGlTextureInfo tex = evalContIO do
    glInfo' <- useAlloca
    tex' <- useObj tex

    success <- liftIO $ fmap toBool $ gr_backendtexture_get_gl_textureinfo tex' glInfo'

    if success
        then do
            glInfo <- liftIO $ peek glInfo'
            pure $ Just glInfo
        else do
            pure Nothing
