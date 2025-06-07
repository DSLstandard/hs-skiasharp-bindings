module Skia.SKFontManager where

import Data.BCP47 qualified as BCP47
import Data.Char qualified
import Data.Text qualified as T
import Data.Traversable
import Skia.Internal.Prelude
import Skia.SKString qualified as SKString

createDefault :: (MonadIO m) => m SKFontManager
createDefault = evalContIO do
    fmgr <- liftIO $ sk_fontmgr_create_default
    toObject fmgr

-- | Returns Skia's global default font manager object.
getGlobalDefault :: (MonadIO m) => m SKFontManager
getGlobalDefault = evalContIO do
    fmgr <- liftIO sk_fontmgr_ref_default
    toObjectFin sk_fontmgr_unref fmgr

countFamilies :: (MonadIO m) => SKFontManager -> m Int
countFamilies fmgr = evalContIO do
    fmgr' <- useObj fmgr
    liftIO $ fmap fromIntegral $ sk_fontmgr_count_families fmgr'

getFamilyName ::
    (MonadIO m) =>
    SKFontManager ->
    -- | Index
    Int ->
    m T.Text
getFamilyName fmgr index = evalContIO do
    familyNameStr <- SKString.createEmpty

    fmgr' <- useObj fmgr
    familyNameStr' <- useObj familyNameStr

    liftIO $ sk_fontmgr_get_family_name fmgr' (fromIntegral index) familyNameStr'

    familyName <- SKString.getAsText familyNameStr
    disposeObject familyNameStr

    pure familyName

createStyleSet ::
    (MonadIO m) =>
    SKFontManager ->
    -- | Index
    Int ->
    m SKFontStyleSet
createStyleSet fmgr index = evalContIO do
    fmgr' <- useObj fmgr
    liftIO $ toObjectFin sk_fontstyleset_unref =<< sk_fontmgr_create_styleset fmgr' (fromIntegral index)

{- | Returns an empty set if the name is not found.

Passing 'Nothing' as the family name will return the default system family. Note
that most systems don't have a default system family, so passing nullptr will
often result in the empty set.

It is possible that this will return a style set not accessible from
'createStyleSet' due to hidden or auto-activated fonts.
-}
matchFamily ::
    (MonadIO m) =>
    SKFontManager ->
    -- | Family name
    Maybe T.Text ->
    m SKFontStyleSet
matchFamily fmgr familyName = evalContIO do
    fmgr' <- useObj fmgr
    familyName' <- useNullIfNothing useTextAsUtf8CString familyName

    tf' <- liftIO $ sk_fontmgr_match_family fmgr' familyName'

    -- Google Skia's comment: The caller must call unref() on the returned
    -- object if it is not null.
    liftIO $ toObjectFin sk_fontstyleset_unref tf'

{- | Find the closest matching typeface to the specified family anme and style
and returns it. Will return 'Nothing' if no \'good\' match is found.

Passing 'Nothing' as the family name will return the default system font.

It is possible that this will return a style set not accessible from
"createStyleSet' or 'matchFamily' due to hidden or auto-activated fonts.
-}
matchFamilyStyle ::
    (MonadIO m) =>
    SKFontManager ->
    -- | Family name
    Maybe T.Text ->
    SKFontStyle ->
    m (Maybe SKTypeface)
matchFamilyStyle fmgr familyName fontStyle = evalContIO do
    fmgr' <- useObj fmgr
    familyName' <- useNullIfNothing useTextAsUtf8CString familyName
    fontStyle' <- useSKFontStyle fontStyle

    tf' <- liftIO $ sk_fontmgr_match_family_style fmgr' familyName' fontStyle'
    if tf' == nullPtr
        then pure Nothing
        else liftIO $ Just <$> toObjectFin sk_typeface_unref tf'

{- | Use the system fallback to find a typeface for the given character.

Will return 'Nothing' if no family can be found for the character in the
system fallback.

Passing 'Nothing' as the family name will return the default system font.

bcp47[0] is the least significant fallback, bcp47[bcp47Count-1] is the most
significant. If no specified bcp47 codes match, any font with the requested
character will be matched.
-}
matchFamilyStyleCharacter ::
    (MonadIO m) =>
    SKFontManager ->
    -- | Family name
    Maybe T.Text ->
    SKFontStyle ->
    [BCP47.BCP47] ->
    Char ->
    m (Maybe SKTypeface)
matchFamilyStyleCharacter fmgr familyName fontStyle inputBcp47s char = evalContIO do
    fmgr' <- useObj fmgr
    familyName' <- useNullIfNothing useTextAsUtf8CString familyName
    fontStyle' <- useSKFontStyle fontStyle

    bcp47Strings <- for inputBcp47s \bcp47 -> do
        useTextAsUtf8CString (BCP47.toText bcp47)
    (bcp47', bcp47Count) <- ContT $ withArrayLen' bcp47Strings

    tf' <-
        liftIO $
            sk_fontmgr_match_family_style_character
                fmgr'
                familyName'
                fontStyle'
                bcp47'
                (fromIntegral bcp47Count)
                (fromIntegral (Data.Char.ord char))
    if tf' == nullPtr
        then pure Nothing
        else Just <$> toObjectFin sk_typeface_unref tf'

{- | Returns a typeface for the specified file name and TTC index (pass
'Nothing' for none). Returns 'Nothing' if the file is not found, or its contents
are not recognized.
-}
createFromFile ::
    (MonadIO m) =>
    SKFontManager ->
    FilePath ->
    -- | TTC index
    Maybe Int ->
    m (Maybe SKTypeface)
createFromFile fmgr path ttcIndex = evalContIO do
    fmgr' <- useObj fmgr
    path' <- ContT $ withCString path

    tf' <- liftIO $ sk_fontmgr_create_from_file fmgr' path' (maybe 0 fromIntegral ttcIndex)
    if tf' == nullPtr
        then pure Nothing
        else do
            -- Google Skia's comment: The caller must call unref() on the
            -- returned object if it is not null.
            Just <$> toObjectFin sk_typeface_unref tf'

{- | Returns a typeface for the specified stream and TTC index (pass 'Nothing'
for none). Returns 'Nothing' if the file is not found, or its contents are not
recognized.
-}
createFromStream ::
    (MonadIO m, IsSKAssetStream stream) =>
    SKFontManager ->
    stream ->
    -- | TTC index
    Maybe Int ->
    m (Maybe SKTypeface)
createFromStream fmgr (toA SKAssetStream -> stream) ttcIndex = evalContIO do
    fmgr' <- useObj fmgr
    stream' <- useObj stream

    tf' <- liftIO $ sk_fontmgr_create_from_stream fmgr' stream' (maybe 0 fromIntegral ttcIndex)
    if tf' == nullPtr
        then pure Nothing
        else do
            -- Google Skia's comment: The caller must call unref() on the
            -- returned object if it is not null.
            Just <$> toObjectFin sk_typeface_unref tf'

{- | Returns a typeface for the specified data and TTC index (pass 'Nothing' for
none). Returns 'Nothing' if the file is not found, or its contents are not
recognized.
-}
createFromData ::
    (MonadIO m, IsSKStream stream) =>
    SKFontManager ->
    SKData ->
    -- | TTC index
    Maybe Int ->
    m (Maybe SKTypeface)
createFromData fmgr dat ttcIndex = evalContIO do
    fmgr' <- useObj fmgr
    dat' <- useObj dat

    tf' <- liftIO $ sk_fontmgr_create_from_data fmgr' dat' (maybe 0 fromIntegral ttcIndex)
    if tf' == nullPtr
        then pure Nothing
        else do
            -- Google Skia's comment: The caller must call unref() on the
            -- returned object if it is not null.
            Just <$> toObjectFin sk_typeface_unref tf'
