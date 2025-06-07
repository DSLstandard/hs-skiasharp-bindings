module Skia.SKBitmap where

import Data.Kind
import Linear
import Skia.Internal.Prelude
import Skia.Types.Linear
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

{- | Returns pixel address, the base address corresponding to the pixel origin,
and the number of bytes of pixel data.
-}
getPixels ::
    (MonadIO m) =>
    SKBitmap ->
    m (Ptr Word8, Int)
getPixels bitmap = evalContIO do
    bitmap' <- useObj bitmap
    numBytes' <- useAlloca
    baseAddr <- liftIO $ sk_bitmap_get_pixels bitmap' numBytes'
    numBytes <- peekWith id numBytes'
    pure (castPtr baseAddr, fromIntegral numBytes)

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

{- | Sets SkImageInfo to \"info\", and creates 'SKPixelRef' containing pixels
and \"rowBytes\".

\"releaseProc\", if not 'Nothing', is called immediately on failure or when
pixels are no longer referenced.

If SkImageInfo could not be set, or \"rowBytes\" is less than info.minRowBytes():
calls releaseProc if present, calls reset(), and returns false.

Otherwise, if pixels equals nullptr: sets SkImageInfo, calls releaseProc if
present, returns true.

If SkImageInfo is set, pixels is not nullptr, and releaseProc is not nullptr:
when pixels are no longer referenced, calls releaseProc with pixels and context
as parameters.
-}
installPixels ::
    (MonadIO m) =>
    SKBitmap ->
    -- | \"info\"
    SKImageInfo ->
    -- | \"pixels\". Address or pixel storage; may be nullptr
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
