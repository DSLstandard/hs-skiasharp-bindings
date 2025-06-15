module Skia.SKFontManager where

import Control.Monad.Trans.Resource
import Data.BCP47 qualified as BCP47
import Data.Char qualified
import Data.Text qualified as T
import Data.Traversable
import Skia.Internal.Prelude
import Skia.SKString qualified as SKString

createDefault :: Acquire SKFontManager
createDefault =
    mkSKObjectAcquire
        sk_fontmgr_create_default
        sk_fontmgr_unref

-- | Returns Skia's global default font manager object.
getGlobalDefault :: Acquire SKFontManager
getGlobalDefault =
    mkSKObjectAcquire
        sk_fontmgr_ref_default
        sk_fontmgr_unref

countFamilies :: (MonadIO m) => SKFontManager -> m Int
countFamilies fmgr = liftIO do
    fromIntegral <$> sk_fontmgr_count_families (ptr fmgr)

getFamilyName ::
    (MonadIO m) =>
    SKFontManager ->
    -- | Index
    Int ->
    m T.Text
getFamilyName fmgr index = evalContIO do
    familyNameStr <- useAcquire SKString.createEmpty
    liftIO $ sk_fontmgr_get_family_name (ptr fmgr) (fromIntegral index) (ptr familyNameStr)
    SKString.getAsText familyNameStr

createStyleSet ::
    SKFontManager ->
    -- | Index
    Int ->
    Acquire SKFontStyleSet
createStyleSet fmgr index =
    mkSKObjectAcquire
        (sk_fontmgr_create_styleset (ptr fmgr) (fromIntegral index))
        sk_fontstyleset_unref

{- | Returns an empty set if the name is not found.

Passing 'Nothing' as the family name will return the default system family. Note
that most systems don't have a default system family, so passing nullptr will
often result in the empty set.

It is possible that this will return a style set not accessible from
'createStyleSet' due to hidden or auto-activated fonts.
-}
matchFamily ::
    SKFontManager ->
    -- | Family name
    Maybe T.Text ->
    Acquire SKFontStyleSet
matchFamily fmgr familyName =
    -- Google Skia on matchFamily(): Never returns NULL; will return an empty
    -- set if the name is not found.
    mkSKObjectAcquire
        ( evalContIO do
            familyName' <- useNullIfNothing useTextAsUtf8CString familyName
            liftIO $ sk_fontmgr_match_family (ptr fmgr) familyName'
        )
        sk_fontstyleset_unref

{- | Finds the closest matching typeface to the specified family anme and style
and returns it. Will return 'Nothing' if no \'good\' match is found.

Passing 'Nothing' as the family name will return the default system font.

It is possible that this will return a style set not accessible from
"createStyleSet' or 'matchFamily' due to hidden or auto-activated fonts.
-}
matchFamilyStyle ::
    (MonadResource m) =>
    SKFontManager ->
    -- | Family name
    Maybe T.Text ->
    SKFontStyle ->
    m (Maybe (ReleaseKey, SKTypeface))
matchFamilyStyle fmgr familyName fontStyle =
    allocateSKObjectUnlessNull
        ( evalContIO do
            fontStyle' <- useSKFontStyle fontStyle
            familyName' <- useNullIfNothing useTextAsUtf8CString familyName
            liftIO $ sk_fontmgr_match_family_style (ptr fmgr) familyName' fontStyle'
        )
        sk_typeface_unref

{- | Use the system fallback to find a typeface for the given character.

Will return 'Nothing' if no family can be found for the character in the system
fallback.

Passing 'Nothing' as the family name will return the default system font.

bcp47[0] is the least significant fallback, bcp47[bcp47Count-1] is the most
significant. If no specified bcp47 codes match, any font with the requested
character will be matched.
-}
matchFamilyStyleCharacter ::
    (MonadResource m) =>
    SKFontManager ->
    -- | Family name
    Maybe T.Text ->
    SKFontStyle ->
    [BCP47.BCP47] ->
    Char ->
    m (Maybe (ReleaseKey, SKTypeface))
matchFamilyStyleCharacter fmgr familyName fontStyle inputBcp47s char =
    allocateSKObjectUnlessNull
        ( evalContIO do
            familyName' <- useNullIfNothing useTextAsUtf8CString familyName
            fontStyle' <- useSKFontStyle fontStyle

            bcp47Strings <- for inputBcp47s \bcp47 -> useTextAsUtf8CString (BCP47.toText bcp47)
            (bcp47', bcp47Count) <- ContT $ withArrayLen' bcp47Strings

            liftIO $
                sk_fontmgr_match_family_style_character
                    (ptr fmgr)
                    familyName'
                    fontStyle'
                    bcp47'
                    (fromIntegral bcp47Count)
                    (fromIntegral (Data.Char.ord char))
        )
        sk_typeface_unref

{- | Returns a typeface for the specified file name and TTC index (pass
'Nothing' for none). Returns 'Nothing' if the file is not found, or its contents
are not recognized.
-}
createFromFile ::
    (MonadResource m) =>
    SKFontManager ->
    FilePath ->
    -- | TTC index
    Maybe Int ->
    m (Maybe (ReleaseKey, SKTypeface))
createFromFile fmgr path ttcIndex =
    allocateSKObjectUnlessNull
        ( evalContIO do
            path' <- ContT $ withCString path
            liftIO $ sk_fontmgr_create_from_file (ptr fmgr) path' (maybe 0 fromIntegral ttcIndex)
        )
        sk_typeface_unref

{- | Returns a typeface for the specified stream and TTC index (pass 'Nothing'
for none). Returns 'Nothing' if the file is not found, or its contents are not
recognized.
-}
createFromStream ::
    (MonadResource m, IsSKStreamAsset stream) =>
    SKFontManager ->
    stream ->
    -- | TTC index
    Maybe Int ->
    m (Maybe (ReleaseKey, SKTypeface))
createFromStream fmgr (toA SKStreamAsset -> stream) ttcIndex =
    allocateSKObjectUnlessNull
        (sk_fontmgr_create_from_stream (ptr fmgr) (ptr stream) (maybe 0 fromIntegral ttcIndex))
        sk_typeface_unref

{- | Returns a typeface for the specified data and TTC index (pass 'Nothing' for
none). Returns 'Nothing' if the file is not found, or its contents are not
recognized.
-}
createFromData ::
    (MonadResource m, IsSKStream stream) =>
    SKFontManager ->
    SKData ->
    -- | TTC index
    Maybe Int ->
    m (Maybe (ReleaseKey, SKTypeface))
createFromData fmgr dat ttcIndex =
    allocateSKObjectUnlessNull
        (sk_fontmgr_create_from_data (ptr fmgr) (ptr dat) (maybe 0 fromIntegral ttcIndex))
        sk_typeface_unref
