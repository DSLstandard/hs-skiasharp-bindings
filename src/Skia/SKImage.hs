module Skia.SKImage where

import Control.Monad.Trans.Resource
import Linear
import Skia.Internal.Prelude

{- | Creates a CPU-backed SkImage from pixmap, copying the pixel data. As a
result, pixmap pixels may be modified or deleted without affecting SkImage.

SkImage is returned if SkPixmap is valid. Valid SkPixmap parameters include:
dimensions are greater than zero; each dimension fits in 29 bits; SkColorType
and SkAlphaType are valid, and SkColorType is not kUnknown_SkColorType; row
bytes are large enough to hold one row of pixels; pixel address is not nullptr.
-}
createRasterFromPixmapCopy ::
    (MonadResource m) =>
    SKPixmap ->
    m (Maybe (ReleaseKey, SKImage))
createRasterFromPixmapCopy pixmap =
    allocateSKObjectUnlessNull
        (sk_image_new_raster_copy_with_pixmap (ptr pixmap))
        sk_image_unref

{- | Creates CPU-backed SkImage from pixel data described by info. The pixels
data will *not* be copied.

SkImage is returned if SkImageInfo is valid. Valid SkImageInfo parameters
include: dimensions are greater than zero; each dimension fits in 29 bits;
SkColorType and SkAlphaType are valid, and SkColorType is not
kUnknown_SkColorType; rowBytes are large enough to hold one row of pixels;
pixels is not nullptr, and contains enough data for SkImage.
-}
createRasterFromData ::
    (MonadResource m) =>
    -- | info. Contains width, height, SkAlphaType, SkColorType, SkColorSpace
    SKImageInfo ->
    -- | pixels. Address or pixel storage
    Ptr Word8 ->
    -- | rowBytes. Size of pixel row or larger
    Int ->
    m (Maybe (ReleaseKey, SKImage))
createRasterFromData iminfo pixels rowBytes =
    allocateSKObjectUnlessNull
        ( evalContIO do
            iminfo' <- useSKImageInfo iminfo
            liftIO $ sk_image_new_raster_data iminfo' (castPtr pixels) (fromIntegral rowBytes)
        )
        sk_image_unref

{- | Creates CPU-backed SkImage from pixmap, sharing SkPixmap pixels. Pixels
must remain valid and unchanged until rasterReleaseProc is called.
rasterReleaseProc is passed releaseContext when SkImage is deleted or no longer
refers to pixmap pixels.

Pass 'Nothing' for rasterReleaseProc to share SkPixmap without requiring a
callback when SkImage is released.

SkImage is returned if pixmap is valid. Valid SkPixmap parameters include:
dimensions are greater than zero; each dimension fits in 29 bits; SkColorType
and SkAlphaType are valid, and SkColorType is not kUnknown_SkColorType; row
bytes are large enough to hold one row of pixels; pixel address is not nullptr.
-}
createRasterFromPixmap ::
    (MonadResource m) =>
    SKPixmap ->
    -- | rasterReleaseProc. Optional. Release callback
    Maybe (IO ()) ->
    m (Maybe (ReleaseKey, SKImage))
createRasterFromPixmap pixmap onRelease = do
    onRelease' <- case onRelease of
        Nothing -> do
            pure nullFunPtr
        Just onRelease -> do
            (_key, onRelease') <-
                allocateAcquire $
                    mkAutoReleasingFunPtr
                        mkFunPtr'Sk_image_raster_release_proc
                        (\_address _ctx -> onRelease)
            pure onRelease'

    allocateSKObjectUnlessNull
        (sk_image_new_raster (ptr pixmap) onRelease' nullPtr)
        sk_image_unref

{- | Creates a CPU-backed SkImage from bitmap, sharing or copying bitmap pixels.
If the bitmap is marked immutable, and its pixel memory is shareable, it may be
shared instead of copied.

SkImage is returned if bitmap is valid. Valid SkBitmap parameters include:
dimensions are greater than zero; each dimension fits in 29 bits;

SkColorType and SkAlphaType are valid, and SkColorType is not
kUnknown_SkColorType; row bytes are large enough to hold one row of pixels;
pixel address is not nullptr.
-}
createRasterFromBitmap ::
    (MonadResource m) =>
    -- | bitmap. SkImageInfo, row bytes, and pixels
    SKBitmap ->
    m (Maybe (ReleaseKey, SKImage))
createRasterFromBitmap bitmap =
    allocateSKObjectUnlessNull
        (sk_image_new_from_bitmap (ptr bitmap))
        sk_image_unref

{- | Return a SkImage using the encoded data, but attempts to defer decoding
until the image is actually used/drawn. This deferral allows the system to
cache the result, either on the CPU or on the GPU, depending on where the
image is drawn. If memory is low, the cache may be purged, causing the next
draw of the image to have to re-decode.

If alphaType is nullopt, the image's alpha type will be chosen automatically
based on the image format. Transparent images will default to
kPremul_SkAlphaType. If alphaType contains kPremul_SkAlphaType or
kUnpremul_SkAlphaType, that alpha type will be used. Forcing opaque (passing
kOpaque_SkAlphaType) is not allowed, and will return nullptr.

If the encoded format is not supported, 'Nothing' is returned.
-}
createDeferredFromEncodedData ::
    (MonadResource m) =>
    SKData ->
    m (Maybe (ReleaseKey, SKImage))
createDeferredFromEncodedData dat =
    allocateSKObjectUnlessNull
        (sk_image_new_from_encoded (ptr dat))
        sk_image_unref

{- | Creates GPU-backed SkImage from the provided GPU texture associated with
context. GPU texture must stay valid and unchanged until textureReleaseProc
is called by Skia. Skia will call textureReleaseProc with the passed-in
releaseContext when SkImage is deleted or no longer refers to the texture. A
non-null SkImage is returned if format of backendTexture is recognized and
supported. Recognized formats vary by GPU backend.

NOTE from Google Skia: When using a DDL recording context, textureReleaseProc
will be called on the GPU thread after the DDL is played back on the direct
context.
-}
borrowTextureFrom ::
    (MonadResource m, IsGRRecordingContext context) =>
    -- | GPU context
    context ->
    -- | backendTexture. texture residing on GPU
    GRBackendTexture ->
    GRSurfaceOrigin ->
    SKColorType ->
    SKAlphaType ->
    -- | colorSpace. This describes the color space of this image's
    -- contents, as seen after sampling. In general, if the format of the
    -- backend texture is SRGB, some linear colorSpace should be supplied
    -- (e.g., SkColorSpace::MakeSRGBLinear()). If the format of the backend
    -- texture is linear, then the colorSpace should include a description
    -- of the transfer function as well (e.g., SkColorSpace::MakeSRGB()).
    SKColorSpace ->
    -- | textureReleaseProc. function called when texture can be released
    IO () ->
    m (Maybe (ReleaseKey, SKImage))
borrowTextureFrom (toA GRRecordingContext -> ctx) tex origin colorType alphaType colorspace onRelease = do
    (_key, onRelease') <-
        allocateAcquire $
            mkAutoReleasingFunPtr
                mkFunPtr'Sk_image_texture_release_proc
                (\_ctx -> onRelease)

    allocateSKObjectUnlessNull
        ( sk_image_new_from_texture
            (ptr ctx)
            (ptr tex)
            (marshalSKEnum origin)
            (marshalSKEnum colorType)
            (marshalSKEnum alphaType)
            (ptr colorspace)
            onRelease'
            nullPtr
        )
        sk_image_unref

{- | Creates GPU-backed SkImage from backendTexture associated with context.
Skia will assume ownership of the resource and will release it when no longer
needed. A 'Just' SkImage is returned if format of backendTexture is
recognized and supported. Recognized formats vary by GPU backend.
-}
adoptTextureFrom ::
    (MonadResource m, IsGRRecordingContext context) =>
    -- | GPU context
    context ->
    -- | backendTexture. texture residing on GPU
    GRBackendTexture ->
    -- | Origin of backendTexture
    GRSurfaceOrigin ->
    -- | Color type of the resulting image
    SKColorType ->
    -- | Alpha type of the resulting image
    SKAlphaType ->
    -- | Optional. Range of colors.
    Maybe SKColorSpace ->
    m (Maybe (ReleaseKey, SKImage))
adoptTextureFrom (toA GRRecordingContext -> ctx) tex origin colorType alphaType colorspace =
    allocateSKObjectUnlessNull
        ( sk_image_new_from_adopted_texture
            (ptr ctx)
            (ptr tex)
            (marshalSKEnum origin)
            (marshalSKEnum colorType)
            (marshalSKEnum alphaType)
            (ptrOrNull colorspace)
        )
        sk_image_unref

data BitDepth = BitDepth'F16 | BitDepth'U8
    deriving (Show, Ord, Eq, Enum, Bounded)

{- | Creates SkImage from picture. Returned SkImage width and height are set by
dimensions. SkImage draws picture with matrix and paint, set to bitDepth and
colorSpace.

The Picture data is not turned into an image (CPU or GPU) until it is drawn.

If matrix is 'Nothing', draws with identity SkMatrix. If paint is 'Nothing',
draws with default SkPaint. colorSpace may be 'Nothing'.
-}
createDeferredFromPicture ::
    (MonadResource m) =>
    SKPicture ->
    -- | Dimensions
    V2 Int ->
    -- | matrix. Optional. SkMatrix to rotate, scale, translate, and so on
    Maybe (M33 Float) ->
    -- | paint. Optional. SkPaint to apply transparency, filtering, and so on.
    Maybe SKPaint ->
    -- | 8-bit integer or 16-bit float: per component
    BitDepth ->
    -- | colorSpace. Optional. Range of colors
    Maybe SKColorSpace ->
    SKSurfaceProps ->
    m (Maybe (ReleaseKey, SKImage))
createDeferredFromPicture picture dimensions matrix paint bitDepth colorspace surfaceProps =
    allocateSKObjectUnlessNull
        ( evalContIO do
            dimensions' <- useStorable $ toSKISize $ fmap fromIntegral dimensions
            matrix' <- useNullIfNothing useStorable $ fmap toSKMatrix $ matrix
            surfaceProps' <- useSKSurfaceProps surfaceProps
            liftIO $
                sk_image_new_from_picture
                    (ptr picture)
                    dimensions'
                    matrix'
                    (ptrOrNull paint)
                    ( fromBool case bitDepth of
                        BitDepth'F16 -> True
                        BitDepth'U8 -> False
                    )
                    (ptrOrNull colorspace)
                    surfaceProps'
        )
        sk_image_unref

-- | Gets the image width.
getWidth :: (MonadIO m) => SKImage -> m Int
getWidth image = evalContIO do
    image' <- useObj image
    liftIO $ fromIntegral <$> sk_image_get_width image'

-- | Gets the image height.
getHeight :: (MonadIO m) => SKImage -> m Int
getHeight image = evalContIO do
    image' <- useObj image
    liftIO $ fromIntegral <$> sk_image_get_height image'

{- | Returns value unique to image. SkImage contents cannot change after SkImage
is created. Any operation to create a new SkImage will receive generate a new
unique number.
-}
getUniqueId ::
    (MonadIO m) =>
    -- | image
    SKImage ->
    m Word32
getUniqueId image = evalContIO do
    image' <- useObj image
    liftIO $ sk_image_get_unique_id image'

-- | Gets the configured SKAlphaType for the bitmap.
getAlphaType :: (MonadIO m) => SKImage -> m SKAlphaType
getAlphaType image = evalContIO do
    image' <- useObj image
    r <- liftIO $ sk_image_get_alpha_type image'
    unmarshalSKEnumOrDie r

-- | Gets the image color type.
getColorType :: (MonadIO m) => SKImage -> m SKColorType
getColorType image = evalContIO do
    image' <- useObj image
    r <- liftIO $ sk_image_get_color_type image'
    unmarshalSKEnumOrDie r

{- | Returns SkColorSpace, the range of colors, associated with SkImage. The
returned SKColorSpace is immutable.

SkColorSpace returned was passed to an SkImage constructor, or was parsed from
encoded data. SkColorSpace returned may be ignored when SkImage is drawn,
depending on the capabilities of the SkSurface receiving the drawing.
-}
getColorSpace :: (MonadResource m) => SKImage -> m (Maybe (ReleaseKey, SKColorSpace))
getColorSpace image =
    allocateSKObjectUnlessNull
        ( do
            colorspace' <- sk_image_get_colorspace (ptr image)

            -- Google Skia's comment: Returns SkColorSpace, the range of colors,
            -- associated with SkImage. The reference count of SkColorSpace is
            -- unchanged. The returned SkColorSpace is immutable.
            unless (colorspace' == nullPtr) do
                sk_colorspace_ref colorspace'

            pure colorspace'
        )
        sk_colorspace_unref

{- | Gets a value indicating whether the image will be drawn as a mask, with no
intrinsic color of its own
-}
isAlphaOnly :: (MonadIO m) => SKImage -> m Bool
isAlphaOnly image = liftIO do
    toBool <$> sk_image_is_alpha_only (ptr image)

-- | Make a shader with the specified tiling and mipmap sampling.
makeShader ::
    (MonadResource m) =>
    SKImage ->
    -- | X tile mode.
    SKShaderTileMode ->
    -- | Y tile mode.
    SKShaderTileMode ->
    SKSamplingOptions ->
    -- | Optional transform.
    Maybe (M33 Float) ->
    m (ReleaseKey, SKShader)
makeShader image tileX tileY sampling matrix =
    allocateSKObject
        ( evalContIO do
            sampling' <- useStorable $ marshalSKSamplingOptions sampling
            matrix' <- useNullIfNothing useStorable $ fmap toSKMatrix $ matrix
            liftIO $ sk_image_make_shader (ptr image) (marshalSKEnum tileX) (marshalSKEnum tileY) sampling' matrix'
        )
        sk_shader_unref

{- | 'makeRawShader' functions like 'makeShader', but for images that contain
non-color data. This includes images encoding things like normals, material
properties (eg, roughness), heightmaps, or any other purely mathematical data
that happens to be stored in an image. These types of images are useful with
some programmable shaders (see: 'SKRuntimeEffect').

Raw image shaders work like regular image shaders (including filtering and
tiling), with a few major differences:

  * No color space transformation is ever applied (the color space of the image
    is ignored).

  * Images with an alpha type of kUnpremul are *not* automatically
    premultiplied.

  * Bicubic filtering is not supported. If SkSamplingOptions::useCubic is true,
    these factories will return nullptr.
-}
makeRawShader ::
    (MonadResource m) =>
    SKImage ->
    -- | X tile mode.
    SKShaderTileMode ->
    -- | Y tile mode.
    SKShaderTileMode ->
    SKSamplingOptions ->
    -- | Optional transform.
    Maybe (M33 Float) ->
    m (ReleaseKey, SKShader)
makeRawShader image tileX tileY sampling matrix =
    allocateSKObject
        ( evalContIO do
            sampling' <- useStorable $ marshalSKSamplingOptions sampling
            matrix' <- useNullIfNothing useStorable $ fmap toSKMatrix $ matrix
            liftIO $ sk_image_make_raw_shader (ptr image) (marshalSKEnum tileX) (marshalSKEnum tileY) sampling' matrix'
        )
        sk_shader_unref

{- | Returns true if the contents of SkImage was created on or uploaded to GPU
memory, and is available as a GPU texture.
-}
isTextureBacked :: (MonadIO m) => SKImage -> m Bool
isTextureBacked image = evalContIO do
    image' <- useObj image
    liftIO $ toBool <$> sk_image_is_texture_backed image'

{- | Returns true if SkImage is backed by an image-generator or other service
that creates and caches its pixels or texture on-demand.
-}
isLazyGenerated :: (MonadIO m) => SKImage -> m Bool
isLazyGenerated image = evalContIO do
    image' <- useObj image
    liftIO $ toBool <$> sk_image_is_lazy_generated image'

{- | Returns true if SkImage draws on GPU surface associated with context.

Also see 'isValidOnRaster'.

NOTE: SkImage backed by GPU texture may become invalid if associated context is
invalid. lazy image may be invalid and may not draw to raster surface or GPU
surface or both.
-}
isValidOnGPUContext ::
    (MonadIO m, IsGRRecordingContext context) =>
    SKImage ->
    -- | context. GPU context to test.
    context ->
    m Bool
isValidOnGPUContext image (toA GRRecordingContext -> ctx) = liftIO do
    toBool <$> sk_image_is_valid (ptr image) (ptr ctx)

{- | Analogous to 'isValidOnGPUContext', but tests if the 'SKImage' is drawn on
a raster surface.

NOTE: SkImage backed by GPU texture may become invalid if associated context is
invalid. lazy image may be invalid and may not draw to raster surface or GPU
surface or both.
-}
isValidOnRaster :: (MonadIO m) => SKImage -> m Bool
isValidOnRaster image = liftIO do
    toBool <$> sk_image_is_valid (ptr image) nullPtr

{- | Copies SkImage pixel address, row bytes, and SkImageInfo to pixmap, if
address is available, and returns true. If pixel address is not available,
return false and leave pixmap unchanged.
-}
peekPixels ::
    (MonadIO m) =>
    SKImage ->
    -- | pixmap. Storage for pixel state if pixels are readable; otherwise,
    -- ignored.
    SKPixmap ->
    m Bool
peekPixels image pixmap = liftIO do
    toBool <$> sk_image_peek_pixels (ptr image) (ptr pixmap)

{- | Returns 'Nothing' if the read operation fails.

Returns 'Just' along with the output 'SKImageInfo' if the read operation
succeeds.

Copies SkRect of pixels from SkImage to dstPixels. Copy starts at offset (srcX,
srcY), and does not exceed SkImage (width(), height()).

dstInfo specifies width, height, SkColorType, SkAlphaType, and SkColorSpace of
destination. dstRowBytes specifies the gap from one destination row to the next.
Returns true if pixels are copied. Returns 'Nothing' if:

* dstInfo.addr() equals nullptr

* dstRowBytes is less than dstInfo.minRowBytes()

* SkPixelRef is nullptr

Pixels are copied only if pixel conversion is possible. If SkImage SkColorType
is kGray_8_SkColorType, or kAlpha_8_SkColorType; dstInfo.colorType() must match.
If SkImage SkColorType is kGray_8_SkColorType, dstInfo.colorSpace() must match.
If SkImage SkAlphaType is kOpaque_SkAlphaType, dstInfo.alphaType() must match.
If SkImage SkColorSpace is nullptr, dstInfo.colorSpace() must match. Returns
false if pixel conversion is not possible.

srcX and srcY may be negative to copy only top or left of source. Returns false
if width() or height() is zero or negative. Returns false if abs(srcX) >= Image
width(), or if abs(srcY) >= Image height().

If cachingHint is kAllow_CachingHint, pixels may be retained locally.

If cachingHint is kDisallow_CachingHint, pixels are not added to the local
cache.
-}
readPixelsToBuffer ::
    (MonadIO m) =>
    SKImage ->
    -- | dstPixels. Destination pixel storage.
    Ptr Word8 ->
    -- | rowBytes. Destination row length
    Int ->
    -- | (srcX, srcY). Source position.
    V2 Int ->
    SKImageCachingHint ->
    m (Maybe SKImageInfo)
readPixelsToBuffer image dstPixels dstRowBytes (V2 srcX srcY) cachingHint = evalContIO do
    iminfo' <- useAlloca
    success <-
        liftIO $
            sk_image_read_pixels
                (ptr image)
                iminfo'
                (castPtr dstPixels)
                (fromIntegral dstRowBytes)
                (fromIntegral srcX)
                (fromIntegral srcY)
                (marshalSKEnum cachingHint)
    if toBool success
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
