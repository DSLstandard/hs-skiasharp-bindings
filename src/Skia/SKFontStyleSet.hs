module Skia.SKFontStyleSet where

import Data.Text qualified as T
import Skia.Internal.Prelude
import Skia.SKString qualified as SKString

createEmpty :: (MonadIO m) => m SKFontStyleSet
createEmpty = liftIO do
    toObjectFin sk_fontstyleset_unref =<< sk_fontstyleset_create_empty

getCount :: (MonadIO m) => SKFontStyleSet -> m Int
getCount sset = evalContIO do
    sset' <- useObj sset
    liftIO $ fmap fromIntegral $ sk_fontstyleset_get_count sset'

getStyle ::
    (MonadIO m) =>
    SKFontStyleSet ->
    -- | Index. Should be in [0..getCount)
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
            pure $ Nothing
        else do
            dstNameStr <- SKString.createEmpty

            sset' <- useObj sset
            dstNameStr' <- useObj dstNameStr
            dstFontStyle' <- useAllocaSKFontStyle

            liftIO $ sk_fontstyleset_get_style sset' (fromIntegral index) dstFontStyle' dstNameStr'

            -- Read out name
            name <- SKString.getAsText dstNameStr
            disposeObject dstNameStr

            -- Read out font style
            fontStyle <- peekSKFontStyle dstFontStyle'

            pure $ Just (fontStyle, name)

createTypeface ::
    (MonadIO m) =>
    SKFontStyleSet ->
    -- | Index. Should be in [0..getCount)
    Int ->
    -- | Returns 'Nothing' if the input index is invalid.
    m (Maybe SKTypeface)
createTypeface sset index = evalContIO do
    -- NOTE: The getCount check is also done by SkiaSharp. This implementation
    -- follows that.
    count <- getCount sset
    if index < 0 || index >= count
        then do
            pure $ Nothing
        else do
            sset' <- useObj sset
            tf' <- liftIO $ sk_fontstyleset_create_typeface sset' (fromIntegral index)
            Just <$> toObjectFin sk_typeface_unref tf'

matchStyle ::
    (MonadIO m) =>
    SKFontStyleSet ->
    -- | Pattern
    SKFontStyle ->
    m SKTypeface
matchStyle sset pat = evalContIO do
    sset' <- useObj sset
    pat' <- useSKFontStyle pat
    tf' <- liftIO $ sk_fontstyleset_match_style sset' pat'
    toObjectFin sk_typeface_unref tf'
