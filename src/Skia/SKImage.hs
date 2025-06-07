module Skia.SKImage where

import Linear
import Skia.Internal.Prelude

createRasterCopyFromPixmap ::
    (MonadIO m) =>
    SKPixmap ->
    m SKImage
createRasterCopyFromPixmap pixmap = evalContIO do
    pixmap' <- useObj pixmap
    image' <- liftIO $ sk_image_new_raster_copy_with_pixmap pixmap'
    toObjectFin sk_image_unref image'

createRasterCopyFromData ::
    (MonadIO m) =>
    SKImageInfo ->
    -- | Pixel data
    Ptr Word8 ->
    -- | Row bytes
    Int ->
    m SKImage
createRasterCopyFromData iminfo pixels rowBytes = evalContIO do
    iminfo' <- useSKImageInfo iminfo
    image' <- liftIO $ sk_image_new_raster_copy iminfo' (castPtr pixels) (fromIntegral rowBytes)
    toObjectFin sk_image_unref image'

createRasterOnData ::
    (MonadIO m) =>
    SKImageInfo ->
    -- | Pixel data
    Ptr Word8 ->
    -- | Row bytes
    Int ->
    m SKImage
createRasterOnData iminfo pixels rowBytes = evalContIO do
    iminfo' <- useSKImageInfo iminfo
    image' <- liftIO $ sk_image_new_raster_data iminfo' (castPtr pixels) (fromIntegral rowBytes)
    toObjectFin sk_image_unref image'

createRaster ::
    (MonadIO m) =>
    SKPixmap ->
    -- | Release callback
    IO () ->
    m SKImage
createRaster pixmap onRelease = evalContIO do
    funptr' <- liftIO mdo
        funptr' <- liftIO $ mkFunPtr'Sk_image_raster_release_proc \_address _ctx -> do
            onRelease
            freeHaskellFunPtr funptr'
        pure funptr'

    pixmap' <- useObj pixmap
    image' <- liftIO $ sk_image_new_raster pixmap' funptr' nullPtr
    toObjectFin sk_image_unref image'

createFromBitmap :: (MonadIO m) => SKBitmap -> m SKImage
createFromBitmap bitmap = evalContIO do
    bitmap' <- useObj bitmap
    image' <- liftIO $ sk_image_new_from_bitmap bitmap'
    toObjectFin sk_image_unref image'

createFromEncoded :: (MonadIO m) => SKData -> m SKImage
createFromEncoded dat = evalContIO do
    dat' <- useObj dat
    image' <- liftIO $ sk_image_new_from_encoded dat'
    toObjectFin sk_image_unref image'

createFromTexture ::
    (MonadIO m) =>
    GRRecordingContext ->
    GRBackendTexture ->
    GRSurfaceOrigin ->
    SKColorType ->
    SKAlphaType ->
    SKColorSpace ->
    -- | Release callback
    IO () ->
    m SKImage
createFromTexture ctx tex origin colorType alphaType colorspace onRelease = evalContIO do
    funptr' <- liftIO mdo
        funptr' <- mkFunPtr'Sk_image_texture_release_proc \_ctx -> do
            onRelease
            freeHaskellFunPtr funptr'
        pure funptr'

    ctx' <- useObj ctx
    tex' <- useObj tex
    colorspace' <- useObj colorspace
    image' <-
        liftIO $
            sk_image_new_from_texture
                ctx'
                tex'
                (marshalSKEnum origin)
                (marshalSKEnum colorType)
                (marshalSKEnum alphaType)
                colorspace'
                funptr'
                nullPtr
    toObjectFin sk_image_unref image'

createFromAdoptedTexture :: (MonadIO m) => GRRecordingContext -> GRBackendTexture -> GRSurfaceOrigin -> SKColorType -> SKAlphaType -> SKColorSpace -> m SKImage
createFromAdoptedTexture ctx tex origin colorType alphaType colorspace = evalContIO do
    ctx' <- useObj ctx
    tex' <- useObj tex
    colorspace' <- useObj colorspace
    image' <-
        liftIO $
            sk_image_new_from_adopted_texture
                ctx'
                tex'
                (marshalSKEnum origin)
                (marshalSKEnum colorType)
                (marshalSKEnum alphaType)
                colorspace'
    toObjectFin sk_image_unref image'

createFromPicture ::
    (MonadIO m) =>
    SKPicture ->
    -- | Dimensions
    V2 Int ->
    M33 Float ->
    SKPaint ->
    -- | Use floating point bit depth?
    Bool ->
    SKColorSpace ->
    SKSurfaceProps ->
    m SKImage
createFromPicture picture dimensions matrix paint useFloatingPointBitDepth colorspace surfaceProps = evalContIO do
    picture' <- useObj picture
    dimensions' <- useStorable $ toSKISize $ fmap fromIntegral dimensions
    matrix' <- useStorable $ toSKMatrix matrix
    paint' <- useObj paint
    colorspace' <- useObj colorspace
    surfaceProps' <- useSKSurfaceProps surfaceProps
    image' <- liftIO $ sk_image_new_from_picture picture' dimensions' matrix' paint' (fromBool useFloatingPointBitDepth) colorspace' surfaceProps'
    toObjectFin sk_image_unref image'

getWidth :: (MonadIO m) => SKImage -> m Int
getWidth image = evalContIO do
    image' <- useObj image
    liftIO $ fromIntegral <$> sk_image_get_width image'

getHeight :: (MonadIO m) => SKImage -> m Int
getHeight image = evalContIO do
    image' <- useObj image
    liftIO $ fromIntegral <$> sk_image_get_height image'

getUniqueId :: (MonadIO m) => SKImage -> m Word32
getUniqueId image = evalContIO do
    image' <- useObj image
    liftIO $ sk_image_get_unique_id image'

getAlphaType :: (MonadIO m) => SKImage -> m SKAlphaType
getAlphaType image = evalContIO do
    image' <- useObj image
    r <- liftIO $ sk_image_get_alpha_type image'
    unmarshalSKEnumOrDie r

getColorType :: (MonadIO m) => SKImage -> m SKColorType
getColorType image = evalContIO do
    image' <- useObj image
    r <- liftIO $ sk_image_get_color_type image'
    unmarshalSKEnumOrDie r

getColorSpace :: (MonadIO m) => SKImage -> m SKColorSpace
getColorSpace image = evalContIO do
    image' <- useObj image
    colorspace' <- liftIO $ sk_image_get_colorspace image'

    liftIO $ sk_colorspace_ref colorspace'
    toObjectFin sk_colorspace_unref colorspace'

isAlphaOnly :: (MonadIO m) => SKImage -> m Bool
isAlphaOnly image = evalContIO do
    image' <- useObj image
    liftIO $ toBool <$> sk_image_is_alpha_only image'

makeShader ::
    (MonadIO m) =>
    SKImage ->
    -- | X tile mode.
    SKShaderTileMode ->
    -- | Y tile mode.
    SKShaderTileMode ->
    SKSamplingOptions ->
    M33 Float ->
    m SKShader
makeShader image tileX tileY sampling matrix = evalContIO do
    image' <- useObj image
    sampling' <- useStorable $ marshalSKSamplingOptions sampling
    matrix' <- useStorable $ toSKMatrix matrix

    shader' <- liftIO $ sk_image_make_shader image' (marshalSKEnum tileX) (marshalSKEnum tileY) sampling' matrix'
    toObjectFin sk_shader_unref shader'

makeRawShader ::
    (MonadIO m) =>
    SKImage ->
    -- | X tile mode.
    SKShaderTileMode ->
    -- | Y tile mode.
    SKShaderTileMode ->
    SKSamplingOptions ->
    M33 Float ->
    m SKShader
makeRawShader image tileX tileY sampling matrix = evalContIO do
    image' <- useObj image
    sampling' <- useStorable $ marshalSKSamplingOptions sampling
    matrix' <- useStorable $ toSKMatrix matrix

    shader' <- liftIO $ sk_image_make_raw_shader image' (marshalSKEnum tileX) (marshalSKEnum tileY) sampling' matrix'
    toObjectFin sk_shader_unref shader'

isTextureBacked :: (MonadIO m) => SKImage -> m Bool
isTextureBacked image = evalContIO do
    image' <- useObj image
    liftIO $ toBool <$> sk_image_is_texture_backed image'

isLazyGenerated :: (MonadIO m) => SKImage -> m Bool
isLazyGenerated image = evalContIO do
    image' <- useObj image
    liftIO $ toBool <$> sk_image_is_lazy_generated image'

isValid :: (MonadIO m, IsGRRecordingContext context) => SKImage -> context -> m Bool
isValid image (toA GRRecordingContext -> ctx) = evalContIO do
    image' <- useObj image
    ctx' <- useObj ctx
    liftIO $ toBool <$> sk_image_is_valid image' ctx'

-- | Returns False if unsuccessful.
peekPixelsToPixmap ::
    (MonadIO m) =>
    SKImage ->
    -- | Destination pixmap
    SKPixmap ->
    m Bool
peekPixelsToPixmap image pixmap = evalContIO do
    image' <- useObj image
    pixmap' <- useObj pixmap
    liftIO $ toBool <$> sk_image_peek_pixels image' pixmap'

{- | Returns 'Nothing' if the read operation fails.

Returns 'Just' along with the output 'SKImageInfo' if the read operation
succeeds.
-}
readPixelsToBuffer ::
    (MonadIO m) =>
    SKImage ->
    -- | Destination pixel buffer
    Ptr Word8 ->
    -- | Destination pixel buffer row bytes
    Int ->
    -- | Source (X, Y) position
    V2 Int ->
    SKImageCachingHint ->
    m (Maybe SKImageInfo)
readPixelsToBuffer image dstPixels dstRowBytes (V2 srcX srcY) cachingHint = evalContIO do
    image' <- useObj image
    iminfo' <- useAlloca
    success <-
        liftIO $
            toBool
                <$> sk_image_read_pixels
                    image'
                    iminfo'
                    (castPtr dstPixels)
                    (fromIntegral dstRowBytes)
                    (fromIntegral srcX)
                    (fromIntegral srcY)
                    (marshalSKEnum cachingHint)

    if success
        then do
            iminfo <- liftIO $ peek iminfo'
            iminfo <- liftIO $ unmarshalSKImageInfo iminfo
            pure $ Just iminfo
        else do
            pure Nothing

-- | Returns false if the read operation fails.
readPixelsToPixmap ::
    (MonadIO m) =>
    SKImage ->
    -- | Destination pixmap.
    SKPixmap ->
    -- | Source (X, Y) position
    V2 Int ->
    SKImageCachingHint ->
    m Bool
readPixelsToPixmap image pixmap (V2 srcX srcY) cachingHint = evalContIO do
    image' <- useObj image
    pixmap' <- useObj pixmap

    liftIO $
        toBool
            <$> sk_image_read_pixels_into_pixmap
                image'
                pixmap'
                (fromIntegral srcX)
                (fromIntegral srcY)
                (marshalSKEnum cachingHint)

-- | Returns false if the operation fails.
scalePixelsToPixmap ::
    (MonadIO m) =>
    SKImage ->
    -- | Destination pixmap.
    SKPixmap ->
    SKSamplingOptions ->
    SKImageCachingHint ->
    m Bool
scalePixelsToPixmap image pixmap sampling cachingHint = evalContIO do
    image' <- useObj image
    pixmap' <- useObj pixmap
    sampling' <- useStorable $ marshalSKSamplingOptions sampling
    liftIO $ toBool <$> sk_image_scale_pixels image' pixmap' sampling' (marshalSKEnum cachingHint)

getEncodedData :: (MonadIO m) => SKImage -> m SKData
getEncodedData image = evalContIO do
    image' <- useObj image
    dat' <- liftIO $ sk_image_ref_encoded image'
    toObjectFin sk_data_unref dat'

createSubsetRaster ::
    (MonadIO m) =>
    SKImage ->
    -- | Subset bounds
    Rect Int ->
    m SKImage
createSubsetRaster image subset = evalContIO do
    image' <- useObj image
    subset' <- useStorable $ toSKIRect subset
    newImage' <- liftIO $ sk_image_make_subset_raster image' subset'
    toObjectFin sk_image_unref newImage'

createSubset ::
    (MonadIO m) =>
    SKImage ->
    GRDirectContext ->
    Rect Int ->
    m SKImage
createSubset image ctx subset = evalContIO do
    image' <- useObj image
    ctx' <- useObj ctx
    subset' <- useStorable $ toSKIRect subset
    newImage' <- liftIO $ sk_image_make_subset image' ctx' subset'
    toObjectFin sk_image_unref newImage'

createTextureImage ::
    (MonadIO m) =>
    SKImage ->
    GRDirectContext ->
    -- | Mipmapped?
    Bool ->
    -- | Budgeted?
    Bool ->
    m SKImage
createTextureImage image ctx mipmapped budgeted = evalContIO do
    image' <- useObj image
    ctx' <- useObj ctx
    newImage' <- liftIO $ sk_image_make_texture_image image' ctx' (fromBool mipmapped) (fromBool budgeted)
    toObjectFin sk_image_unref newImage'

createNonTextureImage ::
    (MonadIO m) =>
    SKImage ->
    m SKImage
createNonTextureImage image = evalContIO do
    image' <- useObj image
    newImage' <- liftIO $ sk_image_make_non_texture_image image'
    toObjectFin sk_image_unref newImage'

createRasterImage ::
    (MonadIO m) =>
    SKImage ->
    m SKImage
createRasterImage image = evalContIO do
    image' <- useObj image
    newImage' <- liftIO $ sk_image_make_raster_image image'
    toObjectFin sk_image_unref newImage'

createWithFilterRaster ::
    (MonadIO m) =>
    SKImage ->
    SKImageFilter ->
    -- | Subset
    Rect Int ->
    -- | Clip bounds
    Rect Int ->
    -- | Output subset
    Rect Int ->
    -- | Output offset
    V2 Int ->
    m SKImage
createWithFilterRaster image imageFilter subset clipBounds outSubset outOffset = evalContIO do
    image' <- useObj image
    imageFilter' <- useObj imageFilter
    subset' <- useStorable $ toSKIRect subset
    clipBounds' <- useStorable $ toSKIRect clipBounds
    outSubset' <- useStorable $ toSKIRect outSubset
    outOffset' <- useStorable $ toSKIPoint outOffset
    newImage' <- liftIO $ sk_image_make_with_filter_raster image' imageFilter' subset' clipBounds' outSubset' outOffset'
    toObjectFin sk_image_unref newImage'

createWithFilter ::
    (MonadIO m, IsGRRecordingContext context) =>
    SKImage ->
    context ->
    SKImageFilter ->
    -- | Subset
    Rect Int ->
    -- | Clip bounds
    Rect Int ->
    -- | Output subset
    Rect Int ->
    -- | Output offset
    V2 Int ->
    m SKImage
createWithFilter image (toA GRRecordingContext -> ctx) imageFilter subset clipBounds outSubset outOffset = evalContIO do
    image' <- useObj image
    ctx' <- useObj ctx
    imageFilter' <- useObj imageFilter
    subset' <- useStorable $ toSKIRect subset
    clipBounds' <- useStorable $ toSKIRect clipBounds
    outSubset' <- useStorable $ toSKIRect outSubset
    outOffset' <- useStorable $ toSKIPoint outOffset
    newImage' <- liftIO $ sk_image_make_with_filter image' ctx' imageFilter' subset' clipBounds' outSubset' outOffset'
    toObjectFin sk_image_unref newImage'
