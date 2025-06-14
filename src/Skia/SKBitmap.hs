module Skia.SKBitmap where

import Data.Kind
import Data.Maybe
import Linear
import Skia.Internal.Prelude
import Skia.Types.Rect qualified as Rect

{- | Creates an empty 'SKBitmap' without pixels, with 'SKColorType'Unknown',
'SKAlphaType'Unknown', and with a width and height of zero. 'SKPixelRef' origin
is set to (0, 0).

MEMORY MANAGEMENT: The returned 'SKBitmap' does not need manual memory
management. When this 'SKBitmap' goes out of scope, the reference count of
Skia's internal 'SKPixelRef' is decremented.
-}
create :: (MonadIO m) => m SKBitmap
create = evalContIO do
    bitmap' <- liftIO sk_bitmap_new

    -- sk_bitmap_destructor: Decrements SkPixelRef reference count, if
    -- SkPixelRef is not nullptr.
    toObjectFin sk_bitmap_destructor bitmap'

-- | Returns width, height, 'SKAlphaType', 'SKColorType', and 'SKColorSpace'.
getInfo :: (MonadIO m) => SKBitmap -> m SKImageInfo
getInfo bitmap = evalContIO do
    bitmap' <- useObj bitmap
    iminfo' <- useAlloca
    liftIO $ sk_bitmap_get_info bitmap' iminfo'
    unmarshalSKImageInfo =<< peekWith id iminfo'

{- | Exposes the WRITABLE base address corresponding to the pixel origin, and
the number of bytes of pixel data.
-}
withPixels ::
    (MonadIO m) =>
    SKBitmap ->
    (CStringLen -> IO r) ->
    m r
withPixels bitmap f = evalContIO do
    bitmap' <- useObj bitmap
    numBytes' <- useAlloca
    baseAddr <- liftIO $ sk_bitmap_get_pixels bitmap' numBytes'
    numBytes <- peekWith id numBytes'
    liftIO $ f (castPtr baseAddr, fromIntegral numBytes)

{- | Returns row bytes, the interval from one pixel row to the next. Row bytes
is at least as large as: @width * getInfo.bytesPerPixel@.

Returns zero if the bitmap's color type is 'SKColorType\'Unknown'.
-}
getRowBytes :: (MonadIO m) => SKBitmap -> m Int
getRowBytes bitmap = evalContIO do
    bitmap' <- useObj bitmap
    liftIO $ fmap fromIntegral $ sk_bitmap_get_row_bytes bitmap'

-- | Returns minimum memory required for pixel storage.
getByteCount :: (MonadIO m) => SKBitmap -> m Int
getByteCount bitmap = evalContIO do
    bitmap' <- useObj bitmap
    liftIO $ fmap fromIntegral $ sk_bitmap_get_byte_count bitmap'

{- | Resets to its initial state; all fields are set to zero, as if 'SKBitmap'
had been initialized by 'create'.

Sets width, height, row bytes to zero; pixel address to nullptr; 'SKColorType'
to 'SKColorType'Unknown'; and 'SKAlphaType' to 'SKAlphaType\'Unknown'.

If 'SKPixelRef' is allocated, its reference count is decreased by one, releasing
its memory if 'SKBitmap' is the sole owner.
-}
reset :: (MonadIO m) => SKBitmap -> m ()
reset bitmap = evalContIO do
    bitmap' <- useObj bitmap
    liftIO $ sk_bitmap_reset bitmap'

-- | Returns true if 'SKPixelRef' is nullptr.
isNull :: (MonadIO m) => SKBitmap -> m Bool
isNull bitmap = evalContIO do
    bitmap' <- useObj bitmap
    liftIO $ fmap toBool $ sk_bitmap_is_null bitmap'

{- | Returns true if pixels can not change.

Most immutable 'SKBitmap' checks trigger an assert only on debug builds of Skia.
-}
isImmutable :: (MonadIO m) => SKBitmap -> m Bool
isImmutable bitmap = evalContIO do
    bitmap' <- useObj bitmap
    liftIO $ fmap toBool $ sk_bitmap_is_immutable bitmap'

{- | Sets internal flag to mark 'SKBitmap' as immutable. Once set, pixels can
not change. Any other bitmap sharing the same 'SKPixelRef' are also marked as
immutable. Once 'SKPixelRef' is marked immutable, the setting cannot be cleared.

Writing to immutable 'SKBitmap' pixels triggers an assert on debug builds of
Skia.
-}
setImmutable :: (MonadIO m) => SKBitmap -> m ()
setImmutable bitmap = evalContIO do
    bitmap' <- useObj bitmap
    liftIO $ sk_bitmap_set_immutable bitmap'

{- | Replaces pixel values with the input 'SKColor', interpreted as being in the
sRGB 'SKColorSpace'. All pixels contained by the bounds of this bitmap are
affected. If the 'SKColorType' of this bitmap is 'SKColorType\'Gray8' or
'SKColorType\'RGB565', then alpha is ignored; RGB is treated as opaque. If
'SKColorType' is 'SKColorType\'Alpha8', then RGB is ignored.
-}
erase :: (MonadIO m) => SKBitmap -> SKColor -> m ()
erase bitmap color = evalContIO do
    bitmap' <- useObj bitmap
    liftIO $ sk_bitmap_erase bitmap' (coerce color)

{- | Replaces pixel values inside area with the input 'SKColor'. interpreted as
being in the sRGB 'SKColorSpace'. If area does not intersect the bounds of this
bitmap, call has no effect.

If the 'SKColorType' of this bitmap is 'SKColorType\'Gray8' or
'SKColorType\'RGB565', then alpha is ignored; RGB is treated as opaque. If
'SKColorType' is 'SKColorType\'Alpha8', then RGB is ignored.
-}
eraseRect ::
    (MonadIO m) =>
    SKBitmap ->
    SKColor ->
    -- | Rectangle to fill
    Rect Int ->
    m ()
eraseRect bitmap color rect = evalContIO do
    bitmap' <- useObj bitmap
    rect' <- useStorable $ Rect.toSKIRect rect
    liftIO $ sk_bitmap_erase_rect bitmap' (coerce color) rect'

-- | Used by 'getAddr'. See comment on 'getAddr'.
data AddrMode :: Type -> Type where
    AddrMode'Generic :: AddrMode Word8
    AddrMode'8 :: AddrMode Word8
    AddrMode'16 :: AddrMode Word16
    AddrMode'32 :: AddrMode Word32

{- | Returns pixel address at (x, y).

Input is not validated: out of bounds values of x or y, or
'SKColorType\'Unknown', trigger an assert() if the C++ Skia library is built
with @SK_DEBUG@ defined. Returns 'Nothing' if SkColorType is
'SKColorType\'Unknown', or 'SKPixelRef' is nullptr ('isNull' of this bitmap
returns true).

Performs a lookup of pixel size. For generic usages, pass 'AddrMode\'Generic'.
For better performance, pass 'AddrMode\'8', 'AddrMode\'16', 'AddrMode\'32'.

This is a unified interface of the following functions:

    * 'sk_bitmap_get_addr'

    * 'sk_bitmap_get_addr_8'

    * 'sk_bitmap_get_addr_16'

    * 'sk_bitmap_get_addr_32'
-}
getAddr ::
    forall a m.
    (MonadIO m) =>
    SKBitmap ->
    -- (x, y)
    V2 Int ->
    -- | Addressing mode. Specialized options ('AddrMode\'8', 'AddrMode\'16',
    -- 'AddrMode\'32') have performance benefits over 'AddrMode\'Generic'.
    AddrMode a ->
    m (Ptr a)
getAddr bitmap (V2 x y) addrMode = evalContIO do
    bitmap' <- useObj bitmap
    let
        applyArgs :: forall r. (Ptr Sk_bitmap -> CInt -> CInt -> r) -> r
        applyArgs f = f bitmap' (fromIntegral x) (fromIntegral y)

    -- Haskell's typechecker seems to struggle with lax usages of dependent
    -- types. Duplicate code here instead.
    case addrMode of
        AddrMode'Generic -> do
            addr' <- liftIO $ applyArgs sk_bitmap_get_addr
            pure $ castPtr addr'
        AddrMode'8 -> do
            addr' <- liftIO $ applyArgs sk_bitmap_get_addr_8
            pure $ castPtr addr'
        AddrMode'16 -> do
            addr' <- liftIO $ applyArgs sk_bitmap_get_addr_16
            pure $ castPtr addr'
        AddrMode'32 -> do
            addr' <- liftIO $ applyArgs sk_bitmap_get_addr_32
            pure $ castPtr addr'

{- | Returns pixel at (x, y) as unpremultiplied color.

Returns black with alpha if 'SKColorType' is 'SKColorType\'Alpha8'.

Input is not validated: out of bounds values of x or y trigger an assert() if
the C++ Skia library built with @SK_DEBUG@ defined; and returns undefined
values or may crash if @SK_RELEASE@ is defined. Fails if 'SKColorType' is
'SKColorType\'Unknown' or 'getPixels' is nullptr.

'SKColorSpace' in 'SKImageInfo' is ignored. Some color precision may be lost
in the conversion to unpremultiplied color; original pixel data may have
additional precision.

Also see 'getPixelColors'.
-}
getPixelColor ::
    (MonadIO m) =>
    SKBitmap ->
    -- (x, y)
    V2 Int ->
    m SKColor
getPixelColor bitmap (V2 x y) = evalContIO do
    bitmap' <- useObj bitmap
    liftIO $ fmap SKColor $ sk_bitmap_get_pixel_color bitmap' (fromIntegral x) (fromIntegral y)

{- | Copies all pixel colors row-by-row to a destination 'SKColor' array of
length (bitmap width x bitmap height).

For reference, the actual C implementation of this function is as follows, note
that @bmp->getColor@ is the same as 'getPixelColor' of this Haskell library:

@
void sk_bitmap_get_pixel_colors(sk_bitmap_t* cbitmap, sk_color_t* colors) {
    SkBitmap* bmp = AsBitmap(cbitmap);
    int w = bmp->width();
    int h = bmp->height();
    for (int y = 0; y < h; y++) {
        for (int x = 0; x < w; x++) {
            *colors = bmp->getColor(x, y);
            colors++;
        }
    }
}
@
-}
getPixelColors ::
    (MonadIO m) =>
    SKBitmap ->
    -- | Destination 'SKColor' array
    Ptr SKColor ->
    m ()
getPixelColors bitmap dst = evalContIO do
    bitmap' <- useObj bitmap
    liftIO $ sk_bitmap_get_pixel_colors bitmap' (coercePtr dst)

-- | Returns true if SkBitmap is can be drawn.
isReadyToDraw ::
    (MonadIO m) =>
    SKBitmap ->
    -- | Return true if 'getPixels' is not nullptr
    m Bool
isReadyToDraw bitmap = evalContIO do
    bitmap' <- useObj bitmap
    liftIO $ fmap toBool $ sk_bitmap_ready_to_draw bitmap'

{- | Copies SkBitmap pixel address, row bytes, and 'SKImageInfo' to the given
pixmap, if address is available, and returns true. If pixel address is not
available, return false and leave the given pixmap unchanged.

Pixmap contents become invalid on any future change to SKBitmap.
-}
peekPixels ::
    (MonadIO m) =>
    SKBitmap ->
    -- | Destination pixmap storage for pixel state if pixels are readable;
    -- otherwise, ignored
    SKPixmap ->
    -- | Returns true if 'SKBitmap' has direct access to pixels
    m Bool
peekPixels bitmap pixmap = evalContIO do
    bitmap' <- useObj bitmap
    pixmap' <- useObj pixmap
    liftIO $ fmap toBool $ sk_bitmap_peek_pixels bitmap' pixmap'

{- | Marks that pixels in 'SKPixelRef' have changed.

Internally, in the Skia library, subsequent calls to @getGenerationID()@
return a different value.
-}
notifyPixelsChanged :: (MonadIO m) => SKBitmap -> m ()
notifyPixelsChanged bitmap = evalContIO do
    bitmap' <- useObj bitmap
    liftIO $ sk_bitmap_notify_pixels_changed bitmap'

-- | Swaps the fields of the two bitmaps.
swap :: (MonadIO m) => SKBitmap -> SKBitmap -> m ()
swap bitmap1 bitmap2 = evalContIO do
    bitmap1' <- useObj bitmap1
    bitmap2' <- useObj bitmap2
    liftIO $ sk_bitmap_swap bitmap1' bitmap2'

-- | Make a shader with the specified tiling, matrix and sampling.
makeShader ::
    (MonadIO m) =>
    SKBitmap ->
    -- | X Y shader tile mode
    V2 SKShaderTileMode ->
    SKSamplingOptions ->
    -- | Optional local matrix
    Maybe (M33 Float) ->
    m SKShader
makeShader bitmap (V2 tx ty) sampling matrix = evalContIO do
    bitmap' <- useObj bitmap
    sampling' <- useStorable $ marshalSKSamplingOptions sampling
    matrix' <- useNullIfNothing useStorable $ fmap toSKMatrix matrix
    shader' <- liftIO $ sk_bitmap_make_shader bitmap' (marshalSKEnum tx) (marshalSKEnum ty) sampling' matrix'
    toObjectFin sk_shader_unref shader'

{- | Sets 'SKImageInfo' to \"info\", and creates 'SKPixelRef' containing pixels
and \"rowBytes\".

\"releaseProc\", if not 'Nothing', is called immediately on failure or when
pixels are no longer referenced.

If 'SKImageInfo' could not be set, or \"rowBytes\" is less than
'Skia.SKImage.minRowBytes': calls \"releaseProc\" if present, calls 'reset', and
returns false.

Otherwise, if \"pixels\" equals 'nullPtr': sets 'SKImageInfo', calls
\"releaseProc\" if present, returns true.

If 'SKImageInfo' is set, \"pixels\" is not 'nullPtr', and \"releaseProc\" is not
'nullPtr': when \"pixels\" are no longer referenced, calls \"releaseProc\" with
pixels and context as parameters.
-}
installPixels ::
    (MonadIO m) =>
    SKBitmap ->
    -- | \"info\"
    SKImageInfo ->
    -- | \"pixels\". Address or pixel storage; may be 'nullPtr'.
    Ptr Word8 ->
    -- | \"rowBytes\". Size of pixel row or larger
    Int ->
    -- | \"releaseProc\". Release callback; called when pixels can be deleted. Optional.
    Maybe (IO ()) ->
    -- | Returns true if the 'SKImageInfo' of the bitmap is set to the input 'SKImageInfo'
    m Bool
installPixels bitmap iminfo pixels rowBytes releaseCallback = evalContIO do
    bitmap' <- useObj bitmap
    iminfo' <- useSKImageInfo iminfo

    releaseproc' <- case releaseCallback of
        Nothing -> do
            pure nullFunPtr
        Just releaseCallback -> liftIO mdo
            releaseproc' <- mkFunPtr'Sk_bitmap_release_proc \_pixels _ctx -> do
                releaseCallback
                freeHaskellFunPtr releaseproc'
            pure releaseproc'

    liftIO $
        fmap toBool $
            sk_bitmap_install_pixels
                bitmap'
                iminfo'
                (castPtr pixels)
                (fromIntegral rowBytes)
                releaseproc'
                nullPtr

{- | Sets 'SKImageInfo' to 'Skia.SKPixmap.getInfo', and creates 'SKPixelRef'
containing pixels address and row bytes info of the input pixmap.

If 'SKImageInfo' could not be set, or the row bytes of the input pixmap is less
than 'Skia.SKImageInfo.minRowBytes'; calls 'reset', and returns false.

Otherwise, if the pixels address of the input pixmap equals nullptr: sets
'SKImageInfo', returns true.

Caller must ensure that pixmap is valid for the lifetime of 'SKBitmap' and
'SKPixelRef'.
-}
installPixelsWithPixmap :: (MonadIO m) => SKBitmap -> SKPixmap -> m Bool
installPixelsWithPixmap bitmap pixmap = evalContIO do
    bitmap' <- useObj bitmap
    pixmap' <- useObj pixmap
    liftIO $ fmap toBool $ sk_bitmap_install_pixels_with_pixmap bitmap' pixmap'

{- | Sets SkImageInfo to info following the rules in setInfo() and allocates
pixel memory. rowBytes must equal or exceed info.width() times
info.bytesPerPixel(), or equal zero. Pass in 'Nothing' for rowBytes to compute the
minimum valid value.

Returns false and calls reset() if SkImageInfo could not be set, or memory could
not be allocated.

On most platforms, allocating pixel memory may succeed even though there is not
sufficient memory to hold pixels; allocation does not take place until the
pixels are written to. The actual behavior depends on the platform
implementation of malloc().
-}
tryAllocPixels ::
    (MonadIO m) =>
    SKBitmap ->
    -- | Contains width, height, SkAlphaType, SkColorType, SkColorSpace
    SKImageInfo ->
    -- | Size of pixel row or larger; Optional.
    Maybe Int ->
    -- | Returns true if pixel storage is allocated
    m Bool
tryAllocPixels bitmap iminfo rowBytes = evalContIO do
    bitmap' <- useObj bitmap
    iminfo' <- useSKImageInfo iminfo
    liftIO $ fmap toBool $ sk_bitmap_try_alloc_pixels bitmap' iminfo' (fromIntegral $ fromMaybe 0 rowBytes)

{- | Replaces SkPixelRef with pixels, preserving SkImageInfo and rowBytes().
Sets SkPixelRef origin to (0, 0).

If pixels is nullptr, or if info().colorType() equals kUnknown_SkColorType;
release reference to SkPixelRef, and set SkPixelRef to nullptr.

Caller is responsible for handling ownership pixel memory for the lifetime of
SkBitmap and SkPixelRef.
-}
setPixels ::
    (MonadIO m) =>
    SKBitmap ->
    -- | address of pixel storage, managed by caller
    Ptr Word8 ->
    m ()
setPixels bitmap pixels = evalContIO do
    bitmap' <- useObj bitmap
    liftIO $ sk_bitmap_set_pixels bitmap' (castPtr pixels)

{- | Shares SkPixelRef with dst. Pixels are not copied; SkBitmap and dst point
to the same pixels; dst bounds() are set to the intersection of subset
and the original bounds().

subset may be larger than bounds(). Any area outside of bounds() is ignored.

Any contents of dst are discarded.

Return false if:

    * SkPixelRef is nullptr

    * subset does not intersect bounds()
-}
extractSubset ::
    (MonadIO m) =>
    SKBitmap ->
    -- | \"dst\". SkBitmap set to subset
    SKBitmap ->
    -- | \"subset\". Rectangle of pixels to reference
    Rect Int ->
    m Bool
extractSubset bitmap dst subset = evalContIO do
    bitmap' <- useObj bitmap
    dst' <- useObj dst
    subset' <- useStorable $ toSKIRect subset
    liftIO $ fmap toBool $ sk_bitmap_extract_subset bitmap' dst' subset'

{- | Sets dst to alpha described by pixels. Returns false if dst cannot be written to
or dst pixels cannot be allocated.

If paint is not nullptr and contains SkMaskFilter, SkMaskFilter
generates mask alpha from SkBitmap. Uses HeapAllocator to reserve memory for dst
SkPixelRef. Sets offset to top-left position for dst for alignment with SkBitmap;
(0, 0) unless SkMaskFilter generates mask.
-}
extractAlpha ::
    (MonadIO m) =>
    SKBitmap ->
    -- | \"dst\"; holds SkPixelRef to fill with alpha layer
    SKBitmap ->
    -- | \"paint\"; Optional. holds optional SkMaskFilter
    Maybe SKPaint ->
    -- | \"offset\"; Optional, top-left position for dst
    Maybe (V2 Int) ->
    m Bool
extractAlpha bitmap dst paint offset = evalContIO do
    bitmap' <- useObj bitmap
    dst' <- useObj dst
    paint' <- useNullIfNothing useObj paint
    offset' <- useNullIfNothing useStorable $ fmap toSKIPoint $ offset
    liftIO $ fmap toBool $ sk_bitmap_extract_alpha bitmap' dst' paint' offset'
