module Skia.SKPixmap where

import Linear
import Skia.Internal.Prelude

new :: (MonadIO m) => m SKPixmap
new = liftIO do
    pixmap' <- sk_pixmap_new
    toObjectFin sk_pixmap_destructor pixmap'

newWithParams ::
    (MonadIO m) =>
    SKImageInfo ->
    -- | Buffer
    Ptr Word8 ->
    -- | Buffer row bytes
    Int ->
    m SKPixmap
newWithParams iminfo buffer rowBytes = evalContIO do
    iminfo' <- useSKImageInfo iminfo
    pixmap' <- liftIO $ sk_pixmap_new_with_params iminfo' (castPtr buffer) (fromIntegral rowBytes)
    toObjectFin sk_pixmap_destructor pixmap'

reset :: (MonadIO m) => SKPixmap -> m ()
reset pixmap = evalContIO do
    pixmap' <- useObj pixmap
    liftIO $ sk_pixmap_reset pixmap'

resetWithParams ::
    (MonadIO m) =>
    SKPixmap ->
    SKImageInfo ->
    -- | Buffer
    Ptr Word8 ->
    -- | Buffer row bytes
    Int ->
    m ()
resetWithParams pixmap iminfo buffer rowBytes = evalContIO do
    pixmap' <- useObj pixmap
    iminfo' <- useSKImageInfo iminfo
    liftIO $ sk_pixmap_reset_with_params pixmap' iminfo' (castPtr buffer) (fromIntegral rowBytes)

setColorSpace :: (MonadIO m) => SKPixmap -> SKColorSpace -> m ()
setColorSpace pixmap colorspace = evalContIO do
    pixmap' <- useObj pixmap
    colorspace' <- useObj colorspace
    liftIO $ sk_pixmap_set_colorspace pixmap' colorspace'

-- | Returns false if operation fails.
extractSubsetToPixmap ::
    (MonadIO m) =>
    SKPixmap ->
    -- | Destination pixmap
    SKPixmap ->
    -- | Subset
    Rect Int ->
    m Bool
extractSubsetToPixmap pixmap dstPixmap subset = evalContIO do
    pixmap' <- useObj pixmap
    dstPixmap' <- useObj dstPixmap
    subset' <- useStorable $ toSKIRect subset
    liftIO $ toBool <$> sk_pixmap_extract_subset pixmap' dstPixmap' subset'

getInfo :: (MonadIO m) => SKPixmap -> m SKImageInfo
getInfo pixmap = evalContIO do
    pixmap' <- useObj pixmap
    iminfo' <- useAlloca
    liftIO $ sk_pixmap_get_info pixmap' iminfo'
    liftIO $ unmarshalSKImageInfo =<< peek iminfo'

getRowBytes :: (MonadIO m) => SKPixmap -> m Int
getRowBytes pixmap = evalContIO do
    pixmap' <- useObj pixmap
    liftIO $ fromIntegral <$> sk_pixmap_get_row_bytes pixmap'

getColorSpace :: (MonadIO m) => SKPixmap -> m SKColorSpace
getColorSpace pixmap = evalContIO do
    pixmap' <- useObj pixmap
    colorspace' <- liftIO $ sk_pixmap_get_colorspace pixmap'

    liftIO $ sk_colorspace_ref colorspace'
    toObjectFin sk_colorspace_unref colorspace'

computeIsOpaque :: (MonadIO m) => SKPixmap -> m Bool
computeIsOpaque pixmap = evalContIO do
    pixmap' <- useObj pixmap
    liftIO $ toBool <$> sk_pixmap_compute_is_opaque pixmap'

getPixelColorHex ::
    (MonadIO m) =>
    SKPixmap ->
    -- | Pixel position
    V2 Int ->
    m SKColor
getPixelColorHex pixmap (V2 x y) = evalContIO do
    pixmap' <- useObj pixmap
    liftIO $ fmap coerce $ sk_pixmap_get_pixel_color pixmap' (fromIntegral x) (fromIntegral y)

getPixelColor ::
    (MonadIO m) =>
    SKPixmap ->
    -- | Pixel position
    V2 Int ->
    m (RGBA Float)
getPixelColor pixmap (V2 x y) = evalContIO do
    pixmap' <- useObj pixmap
    color' <- useAlloca
    liftIO $ sk_pixmap_get_pixel_color4f pixmap' (fromIntegral x) (fromIntegral y) color'
    liftIO $ fmap fromSKColor4f $ peek color'

getPixelAlpha ::
    (MonadIO m) =>
    SKPixmap ->
    -- | Pixel position
    V2 Int ->
    m Float
getPixelAlpha pixmap (V2 x y) = evalContIO do
    pixmap' <- useObj pixmap
    liftIO $ coerce <$> sk_pixmap_get_pixel_alphaf pixmap' (fromIntegral x) (fromIntegral y)

-- | Uses the writable address of a 'SKPixmap'.
withWritableAddress :: (MonadIO m) => SKPixmap -> (Ptr Word8 -> IO r) -> m r
withWritableAddress pixmap f = evalContIO do
    pixmap' <- useObj pixmap
    addr <- liftIO $ castPtr <$> sk_pixmap_get_writable_addr pixmap'
    liftIO $ f addr

withWritableAddressOfXY ::
    (MonadIO m) =>
    SKPixmap ->
    -- | XY position
    V2 Int ->
    (Ptr Word8 -> IO r) ->
    m r
withWritableAddressOfXY pixmap (V2 x y) f = evalContIO do
    pixmap' <- useObj pixmap
    addr <- liftIO $ castPtr <$> sk_pixmap_get_writeable_addr_with_xy pixmap' (fromIntegral x) (fromIntegral y)
    liftIO $ f addr

{- | Returns 'Nothing' if the operation fails.

Returns 'Just' along with the output 'SKImageInfo' if the operation succeeds.
-}
readPixelsToBuffer ::
    (MonadIO m) =>
    SKPixmap ->
    -- | Destination pixel buffer
    Ptr Word8 ->
    -- | Destination pixel buffer row bytes
    Int ->
    -- | Source (X, Y) position
    V2 Int ->
    m (Maybe SKImageInfo)
readPixelsToBuffer pixmap dstPixels dstRowBytes (V2 srcX srcY) = evalContIO do
    pixmap' <- useObj pixmap
    iminfo' <- useAlloca
    success <-
        liftIO $
            toBool
                <$> sk_pixmap_read_pixels
                    pixmap'
                    iminfo'
                    (castPtr dstPixels)
                    (fromIntegral dstRowBytes)
                    (fromIntegral srcX)
                    (fromIntegral srcY)

    if success
        then do
            iminfo <- liftIO $ peek iminfo'
            iminfo <- liftIO $ unmarshalSKImageInfo iminfo
            pure $ Just iminfo
        else do
            pure Nothing

-- | Returns false if the operation fails.
scalePixelsToPixmap ::
    (MonadIO m) =>
    SKPixmap ->
    -- | Destination pixmap.
    SKPixmap ->
    SKSamplingOptions ->
    m Bool
scalePixelsToPixmap pixmap dstPixmap sampling = evalContIO do
    pixmap' <- useObj pixmap
    dstPixmap' <- useObj dstPixmap
    sampling' <- useStorable $ marshalSKSamplingOptions sampling
    liftIO $ toBool <$> sk_pixmap_scale_pixels pixmap' dstPixmap' sampling'

eraseColorHex ::
    (MonadIO m) =>
    SKPixmap ->
    SKColor ->
    -- | Subset
    Rect Int ->
    m Bool
eraseColorHex pixmap color subset = evalContIO do
    pixmap' <- useObj pixmap
    subset' <- useStorable $ toSKIRect subset
    liftIO $ toBool <$> sk_pixmap_erase_color pixmap' (coerce color) subset'

eraseColor ::
    (MonadIO m) =>
    SKPixmap ->
    RGBA Float ->
    -- | Subset
    Rect Int ->
    m Bool
eraseColor pixmap color subset = evalContIO do
    pixmap' <- useObj pixmap
    color' <- useStorable $ toSKColor4f color
    subset' <- useStorable $ toSKIRect subset
    liftIO $ toBool <$> sk_pixmap_erase_color4f pixmap' color' subset'

data EncoderOptions
    = EncoderOptions'Jpeg SKJpegEncoderOptions
    | EncoderOptions'Png SKPngEncoderOptions
    | EncoderOptions'Webp SKWebpEncoderOptions
    deriving (Show)

-- | Returns false if operation fails.
encodeToStream ::
    (IsSKWStream stream, MonadIO m) =>
    -- | Source pixmap
    SKPixmap ->
    -- | Destination stream
    stream ->
    EncoderOptions ->
    m Bool
encodeToStream pixmap (toA SKWStream -> stream) opts = evalContIO do
    pixmap' <- useObj pixmap
    stream' <- useObj stream

    success <- case opts of
        EncoderOptions'Jpeg opts -> do
            opts' <- useSKJpegEncoderOptions opts
            liftIO $ sk_jpegencoder_encode stream' pixmap' opts'
        EncoderOptions'Png opts -> do
            opts' <- useSKPngEncoderOptions opts
            liftIO $ sk_pngencoder_encode stream' pixmap' opts'
        EncoderOptions'Webp opts -> do
            opts' <- useSKWebpEncoderOptions opts
            liftIO $ sk_webpencoder_encode stream' pixmap' opts'

    pure $ toBool success
