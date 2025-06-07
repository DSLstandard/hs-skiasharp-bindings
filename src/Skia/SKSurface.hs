module Skia.SKSurface where

import Linear
import Skia.Internal.Prelude
import Skia.Types.Rect qualified as Rect

createNull ::
    (MonadIO m) =>
    -- | Width
    Int ->
    -- | Height
    Int ->
    -- | Returns 'Nothing' unless the input dimensions are positive.
    m (Maybe SKSurface)
createNull width height = liftIO do
    surface' <- liftIO $ sk_surface_new_null (fromIntegral width) (fromIntegral height)
    if surface' == nullPtr
        then pure Nothing
        else Just <$> toObjectFin sk_surface_unref surface'

createRaster ::
    (MonadIO m) =>
    SKImageInfo ->
    -- | Row bytes
    Int ->
    Maybe SKSurfaceProps ->
    -- | Returns 'Nothing' unless if parameters are valid and memory was allocated.
    m (Maybe SKSurface)
createRaster iminfo rowBytes surfaceProps = evalContIO do
    iminfo' <- useSKImageInfo iminfo
    surfaceProps' <- useNullIfNothing useSKSurfaceProps surfaceProps
    surface' <- liftIO $ sk_surface_new_raster iminfo' (fromIntegral rowBytes) surfaceProps'
    if surface' == nullPtr
        then pure Nothing
        else Just <$> toObjectFin sk_surface_unref surface'

-- SK_C_API sk_surface_t* sk_surface_new_raster_direct(
createByWrappingPixels ::
    (MonadIO m) =>
    SKImageInfo ->
    -- | The pixel buffer backing the surface.
    Ptr Word8 ->
    -- | Row bytes
    Int ->
    -- | On release callback. You may want to deallocate the pixel buffer here.
    IO () ->
    Maybe SKSurfaceProps ->
    -- | Returns 'Nothing' unless if parameters are valid and memory was allocated.
    m (Maybe SKSurface)
createByWrappingPixels iminfo pixelBuffer rowBytes releaseCallback surfaceProps = evalContIO do
    iminfo' <- useSKImageInfo iminfo
    surfaceProps' <- useNullIfNothing useSKSurfaceProps surfaceProps

    -- rec releaseCallback' <- newFunPtr
    onRelease' <- liftIO do
        rec onRelease' <- mkFunPtr'Sk_surface_raster_release_proc \_pixelsAddress _context -> do
                releaseCallback
                freeHaskellFunPtr onRelease'
        pure onRelease'

    surface' <-
        liftIO $
            sk_surface_new_raster_direct
                iminfo'
                (castPtr pixelBuffer)
                (fromIntegral rowBytes)
                onRelease'
                nullPtr
                surfaceProps'
    if surface' == nullPtr
        then pure Nothing
        else Just <$> toObjectFin sk_surface_unref surface'

createByWrappingBackendTexture ::
    (MonadIO m, IsSubclassOf GRRecordingContext context) =>
    context ->
    GRBackendTexture ->
    GRSurfaceOrigin ->
    -- | Sample count
    Int ->
    SKColorType ->
    Maybe SKColorSpace ->
    Maybe SKSurfaceProps ->
    -- | Returns 'Nothing' unless all parameters are valid.
    m (Maybe SKSurface)
createByWrappingBackendTexture (toA GRRecordingContext -> ctx) tex origin sampleCount colorType colorspace props = evalContIO do
    ctx' <- useObj ctx
    tex' <- useObj tex
    colorspace' <- useNullIfNothing useObj colorspace
    props' <- useNullIfNothing useSKSurfaceProps props
    surface' <-
        liftIO $
            sk_surface_new_backend_texture
                ctx'
                tex'
                (marshalSKEnum origin)
                (fromIntegral sampleCount)
                (marshalSKEnum colorType)
                colorspace'
                props'
    if surface' == nullPtr
        then pure Nothing
        else Just <$> toObjectFin sk_surface_unref surface'

createByWrappingBackendRenderTarget ::
    (MonadIO m, IsSubclassOf GRRecordingContext context) =>
    context ->
    GRBackendRenderTarget ->
    GRSurfaceOrigin ->
    SKColorType ->
    Maybe SKColorSpace ->
    Maybe SKSurfaceProps ->
    -- | Returns 'Nothing' unless all parameters are valid.
    m (Maybe SKSurface)
createByWrappingBackendRenderTarget ctx target origin colorType colorspace props = evalContIO do
    ctx' <- useObj (ctx `asA` GRRecordingContext)
    target' <- useObj target
    colorspace' <- useNullIfNothing useObj colorspace
    props' <- useNullIfNothing useSKSurfaceProps props
    surface' <-
        liftIO $
            sk_surface_new_backend_render_target
                ctx'
                target'
                (marshalSKEnum origin)
                (marshalSKEnum colorType)
                colorspace'
                props'
    if surface' == nullPtr
        then pure Nothing
        else Just <$> toObjectFin sk_surface_unref surface'

createByRenderTarget ::
    (MonadIO m, IsSubclassOf GRRecordingContext context) =>
    context ->
    -- | Budgeted?
    Bool ->
    SKImageInfo ->
    -- | Sample count
    Int ->
    GRSurfaceOrigin ->
    Maybe SKSurfaceProps ->
    -- | Should create with mipmaps?
    Bool ->
    -- | Returns 'Nothing' unless all parameters are valid.
    m (Maybe SKSurface)
createByRenderTarget ctx budgeted iminfo sampleCount origin surfaceProps shouldCreateWithMipmaps = evalContIO do
    ctx' <- useObj (ctx `asA` GRRecordingContext)
    iminfo' <- useSKImageInfo iminfo
    surfaceProps' <- useNullIfNothing useSKSurfaceProps surfaceProps
    surface' <-
        liftIO $
            sk_surface_new_render_target
                ctx'
                (fromBool budgeted)
                iminfo'
                (fromIntegral sampleCount)
                (marshalSKEnum origin)
                surfaceProps'
                (fromBool shouldCreateWithMipmaps)
    if surface' == nullPtr
        then pure Nothing
        else Just <$> toObjectFin sk_surface_unref surface'

createFromMetalLayer ::
    (MonadIO m, IsSubclassOf GRRecordingContext context) =>
    context ->
    -- | Pointer to Metal layer (Expected to be a @CAMetalLayer*@ in C).
    Ptr () ->
    GRSurfaceOrigin ->
    -- | Sample count
    Int ->
    SKColorType ->
    Maybe SKColorSpace ->
    Maybe SKSurfaceProps ->
    -- | Pointer to drawable to be filled in when this surface is instantiated; may not be nullptr.
    Ptr (Ptr ()) ->
    -- | Returns 'Nothing' unless all parameters are valid.
    m (Maybe SKSurface)
createFromMetalLayer ctx layer origin sampleCount colorType colorspace surfaceProps dstDrawable = evalContIO do
    ctx' <- useObj (ctx `asA` GRRecordingContext)
    colorspace' <- useNullIfNothing useObj colorspace
    surfaceProps' <- useNullIfNothing useSKSurfaceProps surfaceProps
    surface' <-
        liftIO $
            sk_surface_new_metal_layer
                ctx'
                layer
                (marshalSKEnum origin)
                (fromIntegral sampleCount)
                (marshalSKEnum colorType)
                colorspace'
                surfaceProps'
                dstDrawable
    if surface' == nullPtr
        then pure Nothing
        else Just <$> toObjectFin sk_surface_unref surface'

createFromMetalView ::
    (MonadIO m, IsSubclassOf GRRecordingContext context) =>
    context ->
    -- | Pointer to Metal view (Expected to be a @MTKView*@ in C).
    Ptr () ->
    GRSurfaceOrigin ->
    -- | Sample count
    Int ->
    SKColorType ->
    SKColorSpace ->
    Maybe SKSurfaceProps ->
    -- | Returns 'Nothing' unless all parameters are valid.
    m (Maybe SKSurface)
createFromMetalView ctx view origin sampleCount colorType colorspace surfaceProps = evalContIO do
    ctx' <- useObj (ctx `asA` GRRecordingContext)
    colorspace' <- useObj colorspace
    surfaceProps' <- useNullIfNothing useSKSurfaceProps surfaceProps
    surface' <-
        liftIO $
            sk_surface_new_metal_view
                ctx'
                view
                (marshalSKEnum origin)
                (fromIntegral sampleCount)
                (marshalSKEnum colorType)
                colorspace'
                surfaceProps'
    if surface' == nullPtr
        then pure Nothing
        else Just <$> toObjectFin sk_surface_unref surface'

makeImageSnapshot ::
    (MonadIO m) =>
    SKSurface ->
    -- | Optional crop bounds
    Maybe (Rect Int) ->
    m SKImage
makeImageSnapshot surface cropBounds = evalContIO do
    surface' <- useObj surface
    image' <- case cropBounds of
        Nothing -> do
            liftIO $ sk_surface_new_image_snapshot surface'
        Just cropBounds -> do
            cropBounds' <- useStorable $ Rect.toSKIRect cropBounds
            liftIO $ sk_surface_new_image_snapshot_with_crop surface' cropBounds'
    toObjectFin sk_image_unref image'

-- | Draws SkSurface contents to canvas, with its top-left corner at (x, y).
drawToCanvas ::
    (MonadIO m, IsSKCanvas canvas) =>
    SKSurface ->
    canvas ->
    -- | (x, y)
    V2 Float ->
    SKPaint ->
    m ()
drawToCanvas surface (toA SKCanvas -> canvas) (V2 x y) paint = evalContIO do
    surface' <- useObj surface
    canvas' <- useObj canvas
    paint' <- useObj paint
    liftIO $ sk_surface_draw surface' canvas' (coerce x) (coerce y) paint'

peekPixels ::
    (MonadIO m) =>
    SKSurface ->
    -- | Destination pixmap
    SKPixmap ->
    -- | Returns true on success.
    m Bool
peekPixels surface pixmap = evalContIO do
    surface' <- useObj surface
    pixmap' <- useObj pixmap
    liftIO $ fmap toBool $ sk_surface_peek_pixels surface' pixmap'

readPixels ::
    (MonadIO m) =>
    SKSurface ->
    -- | Destination's image info
    SKImageInfo ->
    -- | Destination pixel buffer
    Ptr Word8 ->
    -- | Destination row bytes
    Int ->
    -- | (X, Y) source position
    V2 Int ->
    -- | Returns true on success.
    m Bool
readPixels surface iminfo pixelBuffer rowBytes (V2 x y) = evalContIO do
    surface' <- useObj surface
    iminfo' <- useSKImageInfo iminfo
    liftIO $
        fmap toBool $
            sk_surface_read_pixels
                surface'
                iminfo'
                (castPtr pixelBuffer)
                (fromIntegral rowBytes)
                (fromIntegral x)
                (fromIntegral y)

getProps :: (MonadIO m) => SKSurface -> m SKSurfaceProps
getProps surface = evalContIO do
    surface' <- useObj surface
    props' <- liftIO $ sk_surface_get_props surface'
    peekSKSurfaceProps props'

{- | Returns 'SKCanvas' that draws into 'SKSurface'. Subsequent calls return the
same 'SKCanvas'. SkCanvas returned is managed and owned by 'SKSurface', and is
deleted when 'SKSurface' is deleted.
-}
getCanvas :: (MonadIO m) => SKSurface -> m SKCanvas
getCanvas surface = evalContIO do
    surface' <- useObj surface
    canvas' <- liftIO $ sk_surface_get_canvas surface'
    toObject canvas'

-- | Returns the recording context being used by the SkSurface.
getRecordingContext ::
    (MonadIO m) =>
    SKSurface ->
    -- | Returns the recorder, if available; 'Nothing' otherwise
    m (Maybe GRRecordingContext)
getRecordingContext surface = evalContIO do
    surface' <- useObj surface
    recorder' <- liftIO $ sk_surface_get_recording_context surface'
    if recorder' == nullPtr
        then do
            pure Nothing
        else do
            -- TODO: unref? Need to confirm
            Just <$> toObject recorder'
