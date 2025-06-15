module Skia.SKFontStyleSet where

import Control.Monad.Trans.Resource
import Data.Text qualified as T
import Skia.Internal.Prelude
import Skia.SKString qualified as SKString

-- | Creates a new, empty 'SKFontStyleSet'.
createEmpty :: Acquire SKFontStyleSet
createEmpty =
    mkSKObjectAcquire
        sk_fontstyleset_create_empty
        sk_fontstyleset_unref

-- | Gets the number of font styles in the set.
getCount :: (MonadIO m) => SKFontStyleSet -> m Int
getCount sset = evalContIO do
    sset' <- useObj sset
    liftIO $ fmap fromIntegral $ sk_fontstyleset_get_count sset'

-- | Gets the font style at the specified index.
getStyle ::
    (MonadIO m) =>
    SKFontStyleSet ->
    -- | Index. Should be in [0..'getCount')
    Int ->
    -- | Returns (SKFontStyle, family name). Returns 'Nothing' if the input
    -- index is invalid.
    m (Maybe (SKFontStyle, T.Text))
getStyle sset index = evalContIO do
    -- NOTE: The getCount check is also done by SkiaSharp. This implementation
    -- follows that.
    count <- getCount sset
    if index < 0 || index >= count
        then do
            pure Nothing
        else do
            dstNameStr <- useAcquire SKString.createEmpty
            dstFontStyle' <- useAllocaSKFontStyle

            liftIO $ sk_fontstyleset_get_style (ptr sset) (fromIntegral index) dstFontStyle' (ptr dstNameStr)
            name <- SKString.getAsText dstNameStr
            fontStyle <- peekSKFontStyle dstFontStyle'
            pure $ Just (fontStyle, name)

{- | Creates a new 'SKTypeface' with the style that is the closest match to the
style at the specified index.
-}
createTypeface ::
    (MonadResource m) =>
    SKFontStyleSet ->
    -- | Index. Should be in [0..'getCount')
    Int ->
    -- | Returns 'Nothing' if the input index is invalid.
    m (Maybe (ReleaseKey, SKTypeface))
createTypeface sset index = do
    -- NOTE: The getCount check is also done by SkiaSharp. This implementation
    -- follows that.
    count <- getCount sset
    if index < 0 || index >= count
        then do
            pure Nothing
        else do
            tf <-
                allocateSKObject
                    (sk_fontstyleset_create_typeface (ptr sset) (fromIntegral index))
                    sk_typeface_unref
            pure $ Just tf

{- | Creates a new SKTypeface with a style that is the closest match to the
specified font style.
-}
matchStyle ::
    (MonadResource m) =>
    SKFontStyleSet ->
    SKFontStyle ->
    m (ReleaseKey, SKTypeface)
matchStyle sset pat = do
    allocateSKObject
        ( evalContIO do
            pat' <- useSKFontStyle pat
            liftIO $ sk_fontstyleset_match_style (ptr sset) pat'
        )
        sk_typeface_unref
