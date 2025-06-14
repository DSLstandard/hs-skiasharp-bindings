module Skia.SKFont where

import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BS
import Data.Char
import Data.Text qualified as T
import Data.Text.Foreign qualified as T
import Data.Vector.Storable qualified as VS
import Skia.Internal.Prelude

destroy :: (MonadIO m) => SKFont -> m ()
destroy font = evalContIO do
    font' <- useObj font
    liftIO $ sk_font_delete font'

-- | Creates an 'SKFont' with default internal settings.
create :: (MonadIO m) => m (Owned SKFont)
create = liftIO do
    font' <- sk_font_new
    toObject font'

{- | Creates an 'SKFont' with default internal settings + input SkTypeface and size in points, horizontal scale, and horizontal skew. Horizontal scale emulates condensed
and expanded fonts. Horizontal skew emulates oblique fonts.
-}
createWithValues ::
    (MonadIO m) =>
    SKTypeface ->
    -- | Size in points
    Float ->
    -- | X scale
    --
    -- You may set this to 1.0 as a default.
    Float ->
    -- | X skew
    --
    -- You may set this to 0.0 as a default.
    Float ->
    m (Owned SKFont)
createWithValues tf size scaleX skewX = evalContIO do
    tf' <- useObj tf
    liftIO $
        toObject
            =<< sk_font_new_with_values tf' (coerce size) (coerce scaleX) (coerce skewX)

isForceAutoHinting :: (MonadIO m) => SKFont -> m Bool
isForceAutoHinting font = evalContIO do
    font' <- useObj font
    liftIO $ fmap toBool $ sk_font_is_force_auto_hinting font'

setForceAutoHinting :: (MonadIO m) => SKFont -> Bool -> m ()
setForceAutoHinting font v = evalContIO do
    font' <- useObj font
    liftIO $ sk_font_set_force_auto_hinting font' (fromBool v)

isEmbeddedBitmaps :: (MonadIO m) => SKFont -> m Bool
isEmbeddedBitmaps font = evalContIO do
    font' <- useObj font
    liftIO $ fmap toBool $ sk_font_is_embedded_bitmaps font'

setEmbeddedBitmaps :: (MonadIO m) => SKFont -> Bool -> m ()
setEmbeddedBitmaps font v = evalContIO do
    font' <- useObj font
    liftIO $ sk_font_set_embedded_bitmaps font' (fromBool v)

isSubpixel :: (MonadIO m) => SKFont -> m Bool
isSubpixel font = evalContIO do
    font' <- useObj font
    liftIO $ fmap toBool $ sk_font_is_subpixel font'

setSubpixel :: (MonadIO m) => SKFont -> Bool -> m ()
setSubpixel font v = evalContIO do
    font' <- useObj font
    liftIO $ sk_font_set_subpixel font' (fromBool v)

isLinearMetrics :: (MonadIO m) => SKFont -> m Bool
isLinearMetrics font = evalContIO do
    font' <- useObj font
    liftIO $ fmap toBool $ sk_font_is_linear_metrics font'

setLinearMetrics :: (MonadIO m) => SKFont -> Bool -> m ()
setLinearMetrics font v = evalContIO do
    font' <- useObj font
    liftIO $ sk_font_set_linear_metrics font' (fromBool v)

isEmbolden :: (MonadIO m) => SKFont -> m Bool
isEmbolden font = evalContIO do
    font' <- useObj font
    liftIO $ fmap toBool $ sk_font_is_embolden font'

setEmbolden :: (MonadIO m) => SKFont -> Bool -> m ()
setEmbolden font v = evalContIO do
    font' <- useObj font
    liftIO $ sk_font_set_embolden font' (fromBool v)

isBaselineSnap :: (MonadIO m) => SKFont -> m Bool
isBaselineSnap font = evalContIO do
    font' <- useObj font
    liftIO $ fmap toBool $ sk_font_is_baseline_snap font'

setBaselineSnap :: (MonadIO m) => SKFont -> Bool -> m ()
setBaselineSnap font v = evalContIO do
    font' <- useObj font
    liftIO $ sk_font_set_baseline_snap font' (fromBool v)

getEdging :: (MonadIO m) => SKFont -> m SKFontEdging
getEdging font = evalContIO do
    font' <- useObj font
    liftIO $ unmarshalSKEnumOrDie =<< sk_font_get_edging font'

setEdging :: (MonadIO m) => SKFont -> SKFontEdging -> m ()
setEdging font v = evalContIO do
    font' <- useObj font
    liftIO $ sk_font_set_edging font' (marshalSKEnum v)

getHinting :: (MonadIO m) => SKFont -> m SKFontHinting
getHinting font = evalContIO do
    font' <- useObj font
    liftIO $ unmarshalSKEnumOrDie =<< sk_font_get_hinting font'

setHinting :: (MonadIO m) => SKFont -> SKFontHinting -> m ()
setHinting font v = evalContIO do
    font' <- useObj font
    liftIO $ sk_font_set_hinting font' (marshalSKEnum v)

{- | Returns SkTypeface if set, or 'Nothing'.

This function increments the reference counter of the returned typeface by one,
and decrements the reference counter once the returned typeface is finalized in
Haskell.
-}
getTypeface :: (MonadIO m) => SKFont -> m (Maybe SKTypeface)
getTypeface font = evalContIO do
    font' <- useObj font

    -- NOTE: sk_font_get_typeface already increments the refcnt
    tf' <- liftIO $ sk_font_get_typeface font'

    if tf' == nullPtr
        then do
            pure Nothing
        else do
            Just <$> toObjectFin sk_typeface_unref tf'

{- | Sets SkTypeface to typeface, decreasing SkRefCnt of the previous
SkTypeface. Pass 'Nothing' to clear SkTypeface and use the default typeface.

Increments the reference counter of the input typeface by one by 'SKFont'.
-}
setTypeface :: (MonadIO m) => SKFont -> Maybe SKTypeface -> m ()
setTypeface font tf = evalContIO do
    font' <- useObj font
    tf' <- useNullIfNothing useObj tf
    liftIO $ sk_font_set_typeface font' tf'

-- | Returns text size in points.
getSize :: (MonadIO m) => SKFont -> m Float
getSize font = evalContIO do
    font' <- useObj font
    fmap coerce $ liftIO $ sk_font_get_size font'

{- | Sets text size in points. Has no effect if textSize is not greater than or
equal to zero.
-}
setSize :: (MonadIO m) => SKFont -> Float -> m ()
setSize font v = evalContIO do
    font' <- useObj font
    liftIO $ sk_font_set_size font' (coerce v)

-- | Returns text scale on x-axis. Default value is 1.
getScaleX :: (MonadIO m) => SKFont -> m Float
getScaleX font = evalContIO do
    font' <- useObj font
    liftIO $ fmap coerce $ sk_font_get_scale_x font'

-- | Sets text scale on x-axis. Default value is 1.
setScaleX :: (MonadIO m) => SKFont -> Float -> m ()
setScaleX font v = evalContIO do
    font' <- useObj font
    liftIO $ sk_font_set_scale_x font' (coerce v)

-- | Returns text skew on x-axis. Default value is zero.
getSkewX :: (MonadIO m) => SKFont -> m Float
getSkewX font = evalContIO do
    font' <- useObj font
    liftIO $ fmap coerce $ sk_font_get_skew_x font'

-- | Sets text skew on x-axis. Default value is zero.
setSkewX ::
    (MonadIO m) =>
    SKFont ->
    -- | \"skewX\". Additional shear on x-axis relative to y-axis
    Float ->
    m ()
setSkewX font v = evalContIO do
    font' <- useObj font
    liftIO $ sk_font_set_skew_x font' (coerce v)

{- | Converts text into glyph indices.

Returns the number of glyph indices represented by text. SkTextEncoding
specifies how text represents characters or glyphs. glyphs may be nullptr, to
compute the glyph count.

Does not check text for valid character codes or valid glyph indices.

If byteLength equals zero, returns zero. If byteLength includes a partial
character, the partial character is ignored.

If encoding is SkTextEncoding::kUTF8 and text contains an invalid UTF-8
sequence, zero is returned.

When encoding is SkTextEncoding::kUTF8, SkTextEncoding::kUTF16, or
SkTextEncoding::kUTF32; then each Unicode codepoint is mapped to a single glyph.
This function uses the default character-to-glyph mapping from the SkTypeface
and maps characters not found in the SkTypeface to zero.
-}
textToGlyphsRaw ::
    (MonadIO m) =>
    SKFont ->
    -- | Text data
    BS.ByteString ->
    -- | Text encoding
    SKTextEncoding ->
    -- | Destination glyph array
    Ptr GlyphId ->
    -- | Destination glyph array capacity
    Int ->
    -- | Returns the number of glyphs written to destination glyph array
    m Int
textToGlyphsRaw font textData encoding glyphs maxGlyphsCount = evalContIO do
    (textBytes, textBytesLen) <- ContT $ BS.unsafeUseAsCStringLen textData
    font' <- useObj font
    returnedCount <-
        liftIO $
            sk_font_text_to_glyphs
                font'
                (castPtr textBytes)
                (fromIntegral textBytesLen)
                (marshalSKEnum encoding)
                (castPtr glyphs)
                (fromIntegral maxGlyphsCount)
    pure $ fromIntegral returnedCount

{- | Returns glyph index for Unicode character.

If the character is not supported by the SkTypeface, returns 0.
-}
unicharToGlyph :: (MonadIO m) => SKFont -> Unichar -> m GlyphId
unicharToGlyph font code = evalContIO do
    font' <- useObj font
    liftIO $ GlyphId <$> sk_font_unichar_to_glyph font' code

-- | Like 'unicharToGlyph' by takes a Haskell 'Char'.
charToGlyph :: (MonadIO m) => SKFont -> Char -> m GlyphId
charToGlyph font char = unicharToGlyph font (fromIntegral (ord char))

-- Google Skia does not document 'unicharsToGlyphsRaw'. The comment is an
-- educated guess made by inspecting src/core/SkTypeface.cpp.

-- | Like 'unicharToGlyph' but operates on an array.
unicharsToGlyphsRaw ::
    (MonadIO m) =>
    SKFont ->
    -- | Unichars
    Ptr Unichar ->
    -- | Number of unichars
    Int ->
    -- | Destination glyph array. Its size should be at least the input number
    -- of unichars.
    Ptr GlyphId ->
    m ()
unicharsToGlyphsRaw font unichars numUnichars dstGlyphs = evalContIO do
    font' <- useObj font
    liftIO $
        sk_font_unichars_to_glyphs
            font'
            unichars
            (fromIntegral numUnichars)
            (castPtr dstGlyphs)

-- | Like 'unicharsToGlyphsRaw' but operates on Haskell vectors.
unicharVectorToGlyphs ::
    (MonadIO m) =>
    SKFont ->
    -- | Vector of unichars
    VS.Vector Unichar ->
    -- | Returns the corresponding glyph IDs of the input.
    m (VS.Vector GlyphId)
unicharVectorToGlyphs font unichars = evalContIO do
    let len = VS.length unichars
    unichars' <- useStorableVector unichars

    glyphsArray <- liftIO $ mallocForeignPtrArray len
    liftIO $ withForeignPtr glyphsArray \glyphsArray' -> do
        unicharsToGlyphsRaw font unichars' (fromIntegral len) glyphsArray'

    pure $ VS.unsafeFromForeignPtr0 glyphsArray len

-- | Like 'unicharVectorToGlyphs' but operates on a Haskell String.
stringToGlyphs :: (MonadIO m) => SKFont -> String -> m (VS.Vector GlyphId)
stringToGlyphs font string =
    unicharVectorToGlyphs font $ VS.fromList $ fmap (fromIntegral . ord) string

{- | Modifies path to be the outline of the glyph.

If the glyph has an outline, modifies path to be the glyph's outline and returns
true. The glyph outline may be empty. Degenerate contours in the glyph outline
will be skipped. If glyph is described by a bitmap, returns false and ignores
path parameter.
-}
getPathOfGlyph ::
    (MonadIO m) =>
    SKFont ->
    GlyphId ->
    -- | Destination path
    SKPath ->
    -- | Returns true if the destination path describes the input glyph.
    m Bool
getPathOfGlyph font glyphId dstPath = evalContIO do
    font' <- useObj font
    dstPath' <- useObj dstPath
    liftIO $ fmap toBool $ sk_font_get_path font' (coerce glyphId) dstPath'

{- | Returns SkFontMetrics associated with SkTypeface.

The return value is the recommended spacing between lines: the sum of metrics
descent, ascent, and leading.

Results are scaled by text size but does not take into account dimensions
required by text scale, text skew, fake bold, style stroke, and SkPathEffect.
-}
getMetrics ::
    (MonadIO m) =>
    SKFont ->
    -- | Returns ('FontMetrics', recommended spacing between lines)
    m (FontMetrics, Float)
getMetrics font = evalContIO do
    metrics' <- useAlloca
    font' <- useObj font

    lineSpacing <- liftIO $ fmap coerce $ sk_font_get_metrics font' metrics'
    metrics <- peekWith marshalFontMetrics metrics'

    pure (metrics, lineSpacing)

{- | Returns the advance width of text and the bounding box of text relative to
(0, 0).

The advance is the normal distance to move before drawing additional text.

The paint stroke settings, mask filter, or path effect may modify the bounds.
-}
measureTextRaw ::
    (MonadIO m) =>
    SKFont ->
    -- | Text bytes
    Ptr Word8 ->
    -- | Text byte length
    Int ->
    -- | Text encoding
    SKTextEncoding ->
    -- | Optional paint
    Maybe SKPaint ->
    -- | Returns (the sum of the default advance widths, bounding box relative to (0, 0))
    m (Float, Rect Float)
measureTextRaw font textBytes textBytesLen encoding paint = evalContIO do
    font' <- useObj font
    paint' <- useNullIfNothing useObj paint
    bounds' <- useAlloca
    advance <-
        liftIO $
            fmap coerce $
                sk_font_measure_text
                    font'
                    (castPtr textBytes)
                    (fromIntegral textBytesLen)
                    (marshalSKEnum encoding)
                    bounds'
                    paint'
    bounds <- peekWith fromSKRect bounds'
    pure (advance, bounds)

-- | Like 'measureTextRaw' but takes 'T.Text'.
measureText :: (MonadIO m) => SKFont -> T.Text -> Maybe SKPaint -> m (Float, Rect Float)
measureText font txt paint = liftIO do
    T.withCStringLen txt \(cstr, cstrlen) -> do
        measureTextRaw font (castPtr cstr) cstrlen SKTextEncoding'UTF8 paint

{-
FIXME: Google Skia did not document breakText() in include/core/SkFont.h. The
written comment is an educated guess made by inspecting src/core/SkFont.cpp and
reading
https://learn.microsoft.com/en-us/dotnet/api/skiasharp.skpaint.breaktext?view=skiasharp-2.88
-}

{- | Measure the text buffer, stopping early if the measured width exceeds
maxWidth pixels.
-}
breakTextRaw ::
    (MonadIO m) =>
    SKFont ->
    -- | Text data
    BS.ByteString ->
    -- | Text encoding
    SKTextEncoding ->
    -- | \"maxWidth\"
    Float ->
    -- | Optional 'SKPaint', which may influence the visual sizes of the text.
    Maybe SKPaint ->
    -- | Returns (the number of bytes of text that were measured, the measured
    -- width)
    m (Int, Float)
breakTextRaw font textData encoding maxWidth paint = evalContIO do
    font' <- useObj font
    paint' <- useNullIfNothing useObj paint

    (textBytes, byteLength) <- ContT $ BS.unsafeUseAsCStringLen textData
    measuredWidth' <- useAlloca

    -- NOTE: one can pass 'measuredWidth' == nullPtr to sk_font_break_text to
    -- ignore the result, but judging from the code in Google Skia's
    -- src/core/SkFont.cpp, reading out 'measuredWidth' is a very cheap
    -- operation, so we will not complicate the interface of this function by
    -- always returning 'measuredWidth', whether the user wants it or not.

    numBytesMeasured <-
        liftIO $
            sk_font_break_text
                font'
                (castPtr textBytes)
                (fromIntegral byteLength)
                (marshalSKEnum encoding)
                (coerce maxWidth)
                measuredWidth'
                paint'

    measuredWidth <- peekWith coerce measuredWidth'
    pure (fromIntegral numBytesMeasured, measuredWidth)
