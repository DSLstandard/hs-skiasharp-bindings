module Skia.SKFont where

import Data.Char
import Data.Vector.Storable qualified as VS
import Linear
import Skia.Internal.Prelude

delete :: (MonadIO m) => SKFont -> m ()
delete font = evalContIO do
    font' <- useObj font
    liftIO $ sk_font_delete font'

create :: (MonadIO m) => m SKFont
create = liftIO do
    font' <- sk_font_new
    toObject font'

createWithValues ::
    (MonadIO m) =>
    SKTypeface ->
    -- | Size
    Float ->
    -- | X scale
    Float ->
    -- | X skew
    Float ->
    m SKFont
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

getTypeface :: (MonadIO m) => SKFont -> m SKTypeface
getTypeface font = evalContIO do
    font' <- useObj font
    -- TODO: unref?
    liftIO $ toObject =<< sk_font_get_typeface font'

setTypeface :: (MonadIO m) => SKFont -> SKTypeface -> m ()
setTypeface font tf = evalContIO do
    font' <- useObj font
    tf' <- useObj tf
    liftIO $ sk_font_set_typeface font' tf'

getSize :: (MonadIO m) => SKFont -> m Float
getSize font = evalContIO do
    font' <- useObj font
    fmap coerce $ liftIO $ sk_font_get_size font'

setSize :: (MonadIO m) => SKFont -> Float -> m ()
setSize font v = evalContIO do
    font' <- useObj font
    liftIO $ sk_font_set_size font' (coerce v)

getScaleX :: (MonadIO m) => SKFont -> m Float
getScaleX font = evalContIO do
    font' <- useObj font
    liftIO $ fmap coerce $ sk_font_get_scale_x font'

setScaleX :: (MonadIO m) => SKFont -> Float -> m ()
setScaleX font v = evalContIO do
    font' <- useObj font
    liftIO $ sk_font_set_scale_x font' (coerce v)

getSkewX :: (MonadIO m) => SKFont -> m Float
getSkewX font = evalContIO do
    font' <- useObj font
    liftIO $ fmap coerce $ sk_font_get_skew_x font'

setSkewX :: (MonadIO m) => SKFont -> Float -> m ()
setSkewX font v = evalContIO do
    font' <- useObj font
    liftIO $ sk_font_set_skew_x font' (coerce v)

textToGlyphsRaw ::
    (MonadIO m) =>
    SKFont ->
    -- | Text bytes
    Ptr () ->
    -- | Text byte length
    Int ->
    -- | Text encoding
    SKTextEncoding ->
    -- | Destination glyph array
    Ptr GlyphId ->
    -- | Max glyphs count
    Int ->
    -- | Returns the number of glyphs wrote to destination glyph array
    m Int
textToGlyphsRaw font textBytes textBytesLen encoding glyphs maxGlyphsCount = evalContIO do
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

getGlyphOfUnichar :: (MonadIO m) => SKFont -> Unichar -> m GlyphId
getGlyphOfUnichar font code = evalContIO do
    font' <- useObj font
    liftIO $ GlyphId <$> sk_font_unichar_to_glyph font' code

getGlyphOfChar :: (MonadIO m) => SKFont -> Char -> m GlyphId
getGlyphOfChar font char = getGlyphOfUnichar font (fromIntegral (ord char))

getGlyphsOfUnicharsRaw ::
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
getGlyphsOfUnicharsRaw font unichars numUnichars dstGlyphs = evalContIO do
    font' <- useObj font
    liftIO $
        sk_font_unichars_to_glyphs
            font'
            unichars
            (fromIntegral numUnichars)
            (castPtr dstGlyphs)

getGlyphsOfUnicharVector ::
    (MonadIO m) =>
    SKFont ->
    -- | Vector of unichars
    VS.Vector Unichar ->
    m (VS.Vector GlyphId)
getGlyphsOfUnicharVector font unichars = evalContIO do
    let len = VS.length unichars
    unichars' <- useStorableVector unichars

    glyphsArray <- liftIO $ mallocForeignPtrArray len
    liftIO $ withForeignPtr glyphsArray \glyphsArray' -> do
        getGlyphsOfUnicharsRaw font unichars' (fromIntegral len) glyphsArray'

    pure $ VS.unsafeFromForeignPtr0 glyphsArray len

getGlyphsOfString :: (MonadIO m) => SKFont -> String -> m (VS.Vector GlyphId)
getGlyphsOfString font string =
    getGlyphsOfUnicharVector font $ VS.fromList $ fmap (fromIntegral . ord) string

measureText ::
    (MonadIO m) =>
    SKFont ->
    -- | Text bytes
    Ptr () ->
    -- | Text byte length
    Int ->
    SKTextEncoding ->
    -- | Bounds
    Rect Float ->
    SKPaint ->
    -- | Text width
    m Float
measureText font textBytes textBytesLen encoding bounds paint = evalContIO do
    font' <- useObj font
    paint' <- useObj paint
    bounds' <- useStorable $ toSKRect bounds
    liftIO $
        coerce
            <$> sk_font_measure_text
                font'
                textBytes
                (fromIntegral textBytesLen)
                (marshalSKEnum encoding)
                bounds'
                paint'

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
