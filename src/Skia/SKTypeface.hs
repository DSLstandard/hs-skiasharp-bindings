module Skia.SKTypeface where

import Data.Char
import Data.Text qualified as T
import Data.Vector.Storable qualified as VS
import Skia.Internal.Prelude
import Skia.SKString qualified as SKString

getDefault :: (MonadIO m) => m SKTypeface
getDefault = liftIO do
    tf' <- sk_typeface_ref_default
    toObject tf'

getFontStyle :: (MonadIO m) => SKTypeface -> m SKFontStyle
getFontStyle tf = evalContIO do
    tf' <- useObj tf

    -- I don't know why Mono Skia allocates the SKFontStyle on heap even though
    -- SKFontStyle encoded as a single int32_t.
    --
    -- TODO: Drop sk_typeface_get_fontstyle and use getFont{Weight,Width,Slant}
    -- instead? But could this be faster because reasons?
    style' <- liftIO $ sk_typeface_get_fontstyle tf'
    style <- peekSKFontStyle style'
    liftIO $ sk_fontstyle_delete style'

    pure style

getFontWeight :: (MonadIO m) => SKTypeface -> m FontWeight
getFontWeight tf = evalContIO do
    tf' <- useObj tf
    liftIO $ fmap fromIntegral $ sk_typeface_get_font_weight tf'

getFontWidth :: (MonadIO m) => SKTypeface -> m FontWidth
getFontWidth tf = evalContIO do
    tf' <- useObj tf
    liftIO $ fmap fromIntegral $ sk_typeface_get_font_width tf'

getFontSlant :: (MonadIO m) => SKTypeface -> m SKFontStyleSlant
getFontSlant tf = evalContIO do
    tf' <- useObj tf
    liftIO $ unmarshalSKEnumOrDie =<< sk_typeface_get_font_slant tf'

isFixedPitch :: (MonadIO m) => SKTypeface -> m Bool
isFixedPitch tf = evalContIO do
    tf' <- useObj tf
    liftIO $ fmap toBool $ sk_typeface_is_fixed_pitch tf'

createDefault :: (MonadIO m) => m SKTypeface
createDefault = liftIO do
    tf' <- sk_typeface_create_default
    toObjectFin sk_typeface_unref tf'

createFromName ::
    (MonadIO m) =>
    -- | Family name. If set to 'Nothing', a default will be used.
    Maybe T.Text ->
    SKFontStyle ->
    m SKTypeface
createFromName familyName style = evalContIO do
    -- TODO: familyName: Use (Short)ByteStrings instead?

    -- FIXME: Does 'sk_typeface_create_from_file' actually take in UTF8??

    familyName' <- useNullIfNothing useTextAsUtf8CString familyName
    style' <- useSKFontStyle style
    tf' <- liftIO $ sk_typeface_create_from_name familyName' style'
    -- TODO: Apparently tf' can never be nullptr? Reconfirm.
    toObjectFin sk_typeface_unref tf'

createFromFile ::
    (MonadIO m) =>
    FilePath ->
    -- | Index
    Int ->
    -- | Returns 'Nothing' if the file does not exist, or is not a valid font
    -- file.
    m (Maybe SKTypeface)
createFromFile path index = evalContIO do
    path' <- ContT $ withCString path
    tf' <- liftIO $ sk_typeface_create_from_file path' (fromIntegral index)
    toObjectFinUnlessNull sk_typeface_unref tf'

createFromStream ::
    (MonadIO m, IsSKStreamAsset stream) =>
    -- | Input stream. The ownership of this stream is transferred, so the
    -- caller must not reference it again.
    stream ->
    -- | Index
    Int ->
    -- | Returns 'Nothing', if the stream is not a valid font file.
    m (Maybe SKTypeface)
createFromStream (toA SKStreamAsset -> stream) index = evalContIO do
    stream' <- useObj stream
    tf' <- liftIO $ sk_typeface_create_from_stream stream' (fromIntegral index)
    toObjectFinUnlessNull sk_typeface_unref tf'

createFromData ::
    (MonadIO m) =>
    SKData ->
    -- | Index
    Int ->
    -- | If the data is null, or is not a valid font file,
    -- returns 'Nothing'.
    m (Maybe SKTypeface)
createFromData dat index = evalContIO do
    dat' <- useObj dat
    tf' <- liftIO $ sk_typeface_create_from_data dat' (fromIntegral index)
    toObjectFinUnlessNull sk_typeface_unref tf'

countGlyphs :: (MonadIO m) => SKTypeface -> m Int
countGlyphs tf = evalContIO do
    tf' <- useObj tf
    liftIO $ fmap fromIntegral $ sk_typeface_count_glyphs tf'

countTables :: (MonadIO m) => SKTypeface -> m Int
countTables tf = evalContIO do
    tf' <- useObj tf
    liftIO $ fmap fromIntegral $ sk_typeface_count_tables tf'

getUnitsPerEm :: (MonadIO m) => SKTypeface -> m Int
getUnitsPerEm tf = evalContIO do
    tf' <- useObj tf
    liftIO $ fmap fromIntegral $ sk_typeface_get_units_per_em tf'

getFamilyName :: (MonadIO m) => SKTypeface -> m T.Text
getFamilyName tf = evalContIO do
    tf' <- useObj tf

    str <- liftIO $ toObjectFin sk_string_destructor =<< sk_typeface_get_family_name tf'
    familyName <- SKString.getAsText str
    disposeObject str

    pure familyName

{- | Returns a stream for the contents of the font data and TrueTypeCollection
of index of the stream, or 0 if the stream is not a collection.. Returns
'Nothing' on failure.

The caller is responsible for deleting the stream.
-}
openStream ::
    (MonadIO m) =>
    SKTypeface ->
    m (Maybe (SKStreamAsset, Int))
openStream tf = evalContIO do
    tf' <- useObj tf
    ttcIndex' <- useAlloca
    stream' <- liftIO $ sk_typeface_open_stream tf' ttcIndex'
    if stream' == nullPtr
        then do
            pure Nothing
        else do
            stream <- toObject stream'
            ttcIndex <- peekWith id ttcIndex'
            pure $ Just (stream, fromIntegral ttcIndex)

getGlyphOfUnichar :: (MonadIO m) => SKTypeface -> Unichar -> m GlyphId
getGlyphOfUnichar tf ch = evalContIO do
    tf' <- useObj tf
    liftIO $ fmap coerce $ sk_typeface_unichar_to_glyph tf' ch

getGlyphOfChar :: (MonadIO m) => SKTypeface -> Char -> m GlyphId
getGlyphOfChar tf ch = getGlyphOfUnichar tf (fromIntegral (ord ch))

getGlyphsOfUnicharsRaw ::
    (MonadIO m) =>
    SKTypeface ->
    -- | Unichars
    Ptr Unichar ->
    -- | Number of unichars
    Int ->
    -- | Destination glyph array. Its size should be at least the input number
    -- of unichars.
    Ptr GlyphId ->
    m ()
getGlyphsOfUnicharsRaw tf unichars numUnichars dstGlyphs = evalContIO do
    tf' <- useObj tf
    liftIO $ sk_typeface_unichars_to_glyphs tf' unichars (fromIntegral numUnichars) (castPtr dstGlyphs)

getGlyphsOfUnicharVector ::
    (MonadIO m) =>
    SKTypeface ->
    -- | Vector of unichars
    VS.Vector Unichar ->
    m (VS.Vector GlyphId)
getGlyphsOfUnicharVector tf unichars = evalContIO do
    let len = VS.length unichars
    unichars' <- useStorableVector unichars

    glyphsArray <- liftIO $ mallocForeignPtrArray len
    liftIO $ withForeignPtr glyphsArray \glyphsArray' -> do
        getGlyphsOfUnicharsRaw tf unichars' (fromIntegral len) glyphsArray'

    pure $ VS.unsafeFromForeignPtr0 glyphsArray len

getGlyphsOfString :: (MonadIO m) => SKTypeface -> String -> m (VS.Vector GlyphId)
getGlyphsOfString tf string =
    getGlyphsOfUnicharVector tf $ VS.fromList $ fmap (fromIntegral . ord) string

{- | Returns the list of table tags in the font. Returns 'Nothing' if an error
occured.
-}
getTableTags :: (MonadIO m) => SKTypeface -> m (Maybe (VS.Vector SKFontTableTag))
getTableTags tf = evalContIO do
    tagsCount <- liftIO $ countTables tf
    tagsArray <- liftIO $ mallocForeignPtrArray tagsCount

    tf' <- useObj tf
    tagsArray' <- ContT $ withForeignPtr tagsArray

    returnedCount <- liftIO $ sk_typeface_get_table_tags tf' tagsArray'
    if returnedCount == 0
        then do
            pure Nothing
        else do
            let tags = VS.unsafeFromForeignPtr0 (coerceForeignPtr tagsArray) tagsCount
            pure $ Just tags

-- | Given a table tag, return the size of its contents, or 'Nothing' if not present.
getTableSize :: (MonadIO m) => SKTypeface -> SKFontTableTag -> m (Maybe Int)
getTableSize tf tag = evalContIO do
    tf' <- useObj tf
    sz <- liftIO $ sk_typeface_get_table_size tf' (coerce tag)
    if sz == 0
        then pure Nothing
        else pure (Just (fromIntegral sz))

{- | Copy the contents of a table into data (allocated by the caller). Note that
the contents of the table will be in their native endian order (which for most
truetype tables is big endian).

If the table tag is not found, or there is an error copying the data, then @Left
()@ is returned. If this happens, it is possible that some or all of the memory
pointed to by data may have been written to, even though an error has occured.
-}
getTableDataRaw ::
    (MonadIO m) =>
    SKTypeface ->
    -- | The table tag whose contents are to be copied
    SKFontTableTag ->
    -- | The offset in bytes into the table's contents where the copy should
    -- start from.
    Int ->
    -- | The number of bytes, starting at offset, of table data to copy.
    Int ->
    -- | Storage address where the table contents are copied to.
    Ptr Word8 ->
    -- | Returns the number of bytes actually copied into data. If offset+length
    -- exceeds the table's size, then only the bytes up to the table's size are
    -- actually copied, and this is the value returned. If offset > the table's
    -- size, or tag is not a valid table, then @Left ()@ is returned.
    m (Either () Int)
getTableDataRaw tf tag offset len buffer = evalContIO do
    tf' <- useObj tf
    numBytes <- liftIO $ sk_typeface_get_table_data tf' (coerce tag) (fromIntegral offset) (fromIntegral len) (castPtr buffer)
    if numBytes == 0
        then do
            pure (Left ())
        else do
            pure (Right (fromIntegral numBytes))

{- | Return an immutable copy of the requested font table, or 'Nothing' if that
table was not found. This can sometimes be faster than calling 'getTableData'
twice: once to find the length, and then again to copy the data.
-}
copyTableData :: (MonadIO m) => SKTypeface -> SKFontTableTag -> m SKData
copyTableData tf tag = evalContIO do
    tf' <- useObj tf
    dat' <- liftIO $ sk_typeface_copy_table_data tf' (coerce tag)
    toObjectFin sk_data_unref dat'

{- | Returns true if the typeface *might* support kerning.

To elaborate: if this function returns false, then
'getKerningPairAdjustmentsRaw' would always return false (no kerning) for all
possible glyph runs; if this function returns true, then
'getKerningPairAdjustmentsRaw' *may* return true for *some* glyph runs.
-}
isKerningSupported :: (MonadIO m) => SKTypeface -> m Bool
isKerningSupported tf = evalContIO do
    -- Google Skia's comment:
    --
    -- Some typefaces are known to never support kerning. Calling this method
    -- with all zeros (e.g. getKerningPairAdustments(NULL, 0, NULL)) returns a
    -- boolean indicating if the typeface might support kerning. If it returns
    -- false, then it will always return false (no kerning) for all possible
    -- glyph runs. If it returns true, then it *may* return true for somne glyph
    -- runs.
    tf' <- useObj tf
    liftIO $ fmap toBool $ sk_typeface_get_kerning_pair_adjustments tf' nullPtr 0 nullPtr

{- | Given a run of glyphs, return the associated horizontal adjustments.
Adjustments are in \"design units\", which are integers relative to the
typeface's units per em (see 'getUnitsPerEm').

Also see 'isKerningSupported'.
-}
getKerningPairAdjustmentsRaw ::
    (MonadIO m) =>
    SKTypeface ->
    -- | Input glyph run
    VS.Vector GlyphId ->
    -- | Destination storage for store the computed kerning adjustments. The
    -- length of this array should be at least the length of the input glyph run.
    Ptr Int32 ->
    -- | Returns true if kerning can be applied to the given glyph run,
    -- otherwise false.
    m Bool
getKerningPairAdjustmentsRaw tf glyphs dstAdjustments = evalContIO do
    tf' <- useObj tf
    glyphs' <- useStorableVector glyphs
    liftIO $
        fmap toBool $
            sk_typeface_get_kerning_pair_adjustments
                tf'
                (coercePtr glyphs')
                (fromIntegral (VS.length glyphs))
                (coercePtr dstAdjustments)
