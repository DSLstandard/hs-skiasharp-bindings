module Skia.SKCodec where

import Control.Exception
import Data.Vector qualified as V
import Linear
import Skia.Internal.Prelude
import System.IO.Unsafe

minBufferedBytesNeeded :: Int
minBufferedBytesNeeded = fromIntegral $ unsafeDupablePerformIO sk_codec_min_buffered_bytes_needed
{-# NOINLINE minBufferedBytesNeeded #-}

destroy :: (MonadIO m) => SKCodec -> m ()
destroy codec = evalContIO do
    codec' <- useObj codec
    liftIO $ sk_codec_destroy codec'

{- | NOTE: If an 'CodecError' is returned, the input stream is deleted
immediately. Otherwise, the returned 'SKCodec' takes ownership of it, and
will delete it when done with it.
-}
createFromStream :: (MonadIO m) => SKStream -> m (Either SKCodecResult SKCodec)
createFromStream stream = evalContIO do
    stream' <- useObj stream
    result' <- useAlloca

    codec' <- liftIO $ sk_codec_new_from_stream stream' result'
    result <- unmarshalSKEnumOrDie =<< peekWith id result'

    case result of
        SKCodecResult'Success -> do
            -- Success
            Right <$> toObject codec'
        _ -> do
            -- If it is not success, then result should indicate an error.
            pure $ Left result

{- | If the input data represents an encoded image that we know how to decode,
return an 'SKCodec' that can decode it. Otherwise return 'Nothing'.
-}
createFromData :: (MonadIO m) => SKData -> m (Maybe SKCodec)
createFromData dat = evalContIO do
    dat' <- useObj dat
    liftIO $ toObjectMaybe =<< sk_codec_new_from_data dat'

{- | Returns a reasonable 'SKImageInfo' to decode into.

If the image has an ICC profile that does not map to an 'SKColorSpace', the
returned 'SKImageInfo' will use SRGB.
-}
getInfo :: (MonadIO m) => SKCodec -> m SKImageInfo
getInfo codec = evalContIO do
    codec' <- useObj codec
    iminfo' <- useAlloca
    liftIO $ sk_codec_get_info codec' iminfo'
    iminfo <- peekWith id iminfo'
    unmarshalSKImageInfo iminfo

getOrigin :: (MonadIO m) => SKCodec -> m SKEncodedOrigin
getOrigin codec = evalContIO do
    codec' <- useObj codec
    liftIO $ unmarshalSKEnumOrDie =<< sk_codec_get_origin codec'

getScaledDimensions ::
    (MonadIO m) =>
    SKCodec ->
    -- | Desired scale
    Float ->
    m (V2 Int)
getScaledDimensions codec desiredScale = evalContIO do
    codec' <- useObj codec
    dimensions' <- useAlloca
    liftIO $ sk_codec_get_scaled_dimensions codec' (coerce desiredScale) dimensions'
    peekWith fromSKISize dimensions'

getValidSubset ::
    (MonadIO m) =>
    SKCodec ->
    m (Maybe (Rect Int))
getValidSubset codec = evalContIO do
    codec' <- useObj codec
    rect' <- useAlloca
    ok <- liftIO $ fmap toBool $ sk_codec_get_valid_subset codec' rect'
    if ok
        then do
            Just <$> peekWith fromSKIRect rect'
        else do
            pure Nothing

getEncodedFormat :: (MonadIO m) => SKCodec -> m SKEncodedImageFormat
getEncodedFormat codec = evalContIO do
    codec' <- useObj codec
    liftIO $ unmarshalSKEnumOrDie =<< sk_codec_get_encoded_format codec'

getPixels ::
    (MonadIO m) =>
    SKCodec ->
    SKImageInfo ->
    -- | Destination pixel data
    Ptr Word8 ->
    -- | Destination row bytes
    Int ->
    -- | Optional codec options
    Maybe SKCodecOptions ->
    m SKCodecResult
getPixels codec iminfo pixelData rowBytes codecOptions = evalContIO do
    codec' <- useObj codec
    iminfo' <- useSKImageInfo iminfo
    codecOptions' <- useNullIfNothing useSKCodecOptions codecOptions
    liftIO $
        unmarshalSKEnumOrDie
            =<< sk_codec_get_pixels
                codec'
                iminfo'
                (castPtr pixelData)
                (fromIntegral rowBytes)
                codecOptions'

startIncrementalDecode ::
    (MonadIO m) =>
    SKCodec ->
    -- | Destination's image info
    SKImageInfo ->
    -- | Destination pixel buffer
    Ptr Word8 ->
    -- | Destination row bytes
    Int ->
    -- | Optional codec options
    Maybe SKCodecOptions ->
    m SKCodecResult
startIncrementalDecode codec iminfo pixelData rowBytes codecOptions = evalContIO do
    codec' <- useObj codec
    iminfo' <- useSKImageInfo iminfo
    codecOptions' <- useNullIfNothing useSKCodecOptions codecOptions
    liftIO $
        unmarshalSKEnumOrDie
            =<< sk_codec_start_incremental_decode
                codec'
                iminfo'
                (castPtr pixelData)
                (fromIntegral rowBytes)
                codecOptions'

data IncrementalDecodeResult
    = -- | Contains an 'Int' describing the number of rows decoded.
      IncrementalDecodeResult'Progress Int
    | IncrementalDecodeResult'Done
    deriving (Show, Eq, Ord)

incrementalDecode ::
    (MonadIO m) =>
    SKCodec ->
    -- | Returns (Number of rows decoded, Codec result)
    m IncrementalDecodeResult
incrementalDecode codec = evalContIO do
    codec' <- useObj codec
    numRowsDecoded' <- useAlloca

    result <-
        liftIO $
            unmarshalSKEnumOrDie
                =<< sk_codec_incremental_decode codec' numRowsDecoded'

    case result of
        SKCodecResult'IncompleteInput -> do
            numRowsDecoded <- peekWith fromIntegral numRowsDecoded'
            pure $ IncrementalDecodeResult'Progress numRowsDecoded
        SKCodecResult'Success -> do
            pure $ IncrementalDecodeResult'Done
        _ -> do
            liftIO $ throwIO $ InternalError $ "Expects either Complete or IncompleteInput, but got " <> show result

startScanlineDecode ::
    (MonadIO m) =>
    SKCodec ->
    -- | Destination's image info
    SKImageInfo ->
    -- | Optional codec options
    Maybe SKCodecOptions ->
    m SKCodecResult
startScanlineDecode codec iminfo options = evalContIO do
    codec' <- useObj codec
    iminfo' <- useSKImageInfo iminfo
    options' <- useNullIfNothing useSKCodecOptions options
    liftIO $ unmarshalSKEnumOrDie =<< sk_codec_start_scanline_decode codec' iminfo' options'

-- | Writes the next \"countLines\" scanlines into the destination pixel buffer.
getScanlines ::
    (MonadIO m) =>
    SKCodec ->
    -- | Destination pixel buffer
    Ptr Word8 ->
    -- | \"countLines\"
    Int ->
    -- | Pixel buffer row bytes
    Int ->
    -- | Returns the number of lines successfully decoded. If this value is less than
    -- countLines, this will fill the remaining lines with a default value.
    m Int
getScanlines codec dst countLines rowBytes = evalContIO do
    codec' <- useObj codec
    liftIO $
        fmap fromIntegral $
            sk_codec_get_scanlines
                codec'
                (castPtr dst)
                (fromIntegral countLines)
                (fromIntegral rowBytes)

skipScanlines ::
    (MonadIO m) =>
    SKCodec ->
    -- | Line count
    Int ->
    -- | True if the scanlines were successfully skipped.
    --
    -- False on failure, possible reasons for failure include:
    -- - An incomplete input image stream.
    -- - Calling this function before calling startScanlineDecode().
    -- - If countLines is less than zero or so large that it moves the current scanline past the end of the image.
    m Bool
skipScanlines codec countLines = evalContIO do
    codec' <- useObj codec
    liftIO $ fmap toBool $ sk_codec_skip_scanlines codec' (fromIntegral countLines)

{- | An enum representing the order in which scanlines will be returned by the
scanline decoder.

This is undefined before 'startScanlineDecode' is called.
-}
getScanlineOrder :: (MonadIO m) => SKCodec -> m SKCodecScanlineOrder
getScanlineOrder codec = evalContIO do
    codec' <- useObj codec
    liftIO $ unmarshalSKEnumOrDie =<< sk_codec_get_scanline_order codec'

{- | Returns the y-coordinate of the next row to be returned by the scanline
decoder.

This will equal the current scanline value of the codec, except in the case of
strangely encoded image types (bottom-up bmps).

Results are undefined when not in scanline decoding mode.
-}
nextScanline :: (MonadIO m) => SKCodec -> m Int
nextScanline codec = evalContIO do
    codec' <- useObj codec
    liftIO $ fmap fromIntegral $ sk_codec_next_scanline codec'

-- Returns the output y-coordinate of the row that corresponds to an input
-- y-coordinate.  The input y-coordinate represents where the scanline is
-- located in the encoded data.
--
-- This will equal inputScanline, except in the case of strangely encoded image
-- types (bottom-up bmps, interlaced gifs).
outputScanline ::
    (MonadIO m) =>
    SKCodec ->
    -- | \"inputScanline\"
    Int ->
    m Int
outputScanline codec inputScanline = evalContIO do
    codec' <- useObj codec
    liftIO $ fmap fromIntegral $ sk_codec_output_scanline codec' (fromIntegral inputScanline)

getFrameCount :: (MonadIO m) => SKCodec -> m Int
getFrameCount codec = evalContIO do
    codec' <- useObj codec
    liftIO $ fmap fromIntegral $ sk_codec_get_frame_count codec'

{- | Return info about a single frame.

Does not read through the stream, so it should be called after
'getFrameCount' to parse any frames that have not already been parsed.

Only supported by animated (multi-frame) codecs. Note that this is a property
of the codec (the SKCodec subclass), not the image.

To elaborate, some codecs support animation (e.g. GIF). Others do not (e.g.
BMP). Animated codecs can still represent single frame images. Calling
@getFrameInfo codec 0@ will return true for a single frame GIF even if the
overall image is not animated (in that the pixels on screen do not change
over time). When incrementally decoding a GIF image, we might only know that
there's a single frame *so far*.

For non-animated SKCodec subclasses, it's sufficient but not necessary for
this method to always return false.
-}
getFrameInfoForIndex ::
    (MonadIO m) =>
    SKCodec ->
    -- | Frame index
    Int ->
    -- | Returns 'Nothing' if the operation fails (when, e.g., the input index
    -- is negative).
    m (Maybe SKCodecFrameInfo)
getFrameInfoForIndex codec frameIndex = evalContIO do
    codec' <- useObj codec
    frameinfo' <- useAlloca
    ok <-
        liftIO $
            fmap toBool $
                sk_codec_get_frame_info_for_index
                    codec'
                    (fromIntegral frameIndex)
                    frameinfo'
    if ok
        then do
            frameinfo <- peekWith id frameinfo'
            Just <$> unmarshalSKCodecFrameInfo frameinfo
        else do
            pure Nothing

{- | Return info about all the frames in the image.

May require reading through the stream to determine info about the frames
(including the count).

As such, future decoding calls may require a rewind.

This may return an empty vector for non-animated codecs. See the
'getFrameInfoForIndex' comment.
-}
getFrameInfoForAll :: (MonadIO m) => SKCodec -> m (V.Vector SKCodecFrameInfo)
getFrameInfoForAll codec = evalContIO do
    -- We probably should use 'sk_codec_get_frame_info' instead of iterating on
    -- 'sk_codec_get_frame_info_for_index' because of optimizations done on the
    -- side of C/C++... but at the cost of more memory allocation, which is
    -- likely negligible.

    codec' <- useObj codec
    count <- liftIO $ fmap fromIntegral $ sk_codec_get_frame_count codec'

    frameinfos' <- useAllocaArray count
    liftIO $ sk_codec_get_frame_info codec' frameinfos'

    -- TODO: This looks inefficient
    frameinfos <- liftIO $ peekArray count frameinfos'
    frameinfos <- traverse unmarshalSKCodecFrameInfo frameinfos
    pure $ V.fromListN count frameinfos

{- | Return the number of times to repeat, if this image is animated. This
number does not include the first play through of each frame. For example, a
repetition count of 4 means that each frame is played 5 times and then the
animation stops.

It can return kRepetitionCountInfinite, a negative number, meaning that the
animation should loop forever.

May require reading the stream to find the repetition count.

As such, future decoding calls may require a rewind.

For still (non-animated) image codecs, this will return 0.
-}
getRepetitionCount :: (MonadIO m) => SKCodec -> m Int
getRepetitionCount codec = evalContIO do
    codec' <- useObj codec
    liftIO $ fmap fromIntegral $ sk_codec_get_repetition_count codec'
