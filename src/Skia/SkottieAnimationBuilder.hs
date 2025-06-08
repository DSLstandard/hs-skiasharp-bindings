module Skia.SkottieAnimationBuilder where

import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BS
import Foreign.C.String qualified
import Skia.Internal.Prelude

data CreateFlags = CreateFlags
    { deferImageLoading :: Bool
    -- ^ Normally, all static image frames are resolved at load time via
    -- ImageAsset::getFrame(0).  With this flag, frames are only resolved when
    -- needed, at seek() time.
    , preferEmbeddedFonts :: Bool
    -- ^ Attempt to use the embedded fonts (glyph paths, normally used as
    -- fallback) over native Skia typefaces.
    }
    deriving (Show, Eq, Ord)

defaultCreateFlags :: CreateFlags
defaultCreateFlags =
    CreateFlags
        { deferImageLoading = False
        , preferEmbeddedFonts = False
        }

marshalCreateFlags :: CreateFlags -> Skottie_animation_builder_flags
marshalCreateFlags i =
    makeBitFlags
        [ (i.deferImageLoading, DEFER_IMAGE_LOADING_SKOTTIE_ANIMATION_BUILDER_FLAGS)
        , (i.preferEmbeddedFonts, PREFER_EMBEDDED_FONTS_SKOTTIE_ANIMATION_BUILDER_FLAGS)
        ]

destroy :: (MonadIO m) => SkottieAnimationBuilder -> m ()
destroy builder = evalContIO do
    builder' <- useObj builder
    liftIO $ skottie_animation_builder_delete builder'

create ::
    (MonadIO m) =>
    -- | Additional options. Consider using 'defaultCreateFlags'.
    CreateFlags ->
    m (Owned SkottieAnimationBuilder)
create flags = liftIO do
    builder' <- skottie_animation_builder_new (marshalCreateFlags flags)
    toObject builder'

data Stats = Stats
    { totalLoadTimeMS :: Float
    , jsonParseTimeMS :: Float
    , sceneParseTimeMS :: Float
    , jsonSize :: Int
    , animatorCount :: Int
    }
    deriving (Show, Eq, Ord)

unmarshalStats :: Skottie_animation_builder_stats -> Stats
unmarshalStats Skottie_animation_builder_stats{..} =
    Stats
        { totalLoadTimeMS = coerce fTotalLoadTimeMS
        , jsonParseTimeMS = coerce fJsonParseTimeMS
        , sceneParseTimeMS = coerce fSceneParseTimeMS
        , jsonSize = fromIntegral fJsonSize
        , animatorCount = fromIntegral fAnimatorCount
        }

getStats :: (MonadIO m) => SkottieAnimationBuilder -> m Stats
getStats builder = evalContIO do
    builder' <- useObj builder
    stats' <- useAlloca
    liftIO $ skottie_animation_builder_get_stats builder' stats'
    peekWith unmarshalStats stats'

setResourceProvider :: (MonadIO m) => SkottieAnimationBuilder -> SKResourcesResourceProvider -> m ()
setResourceProvider builder provider = evalContIO do
    builder' <- useObj builder
    provider' <- useObj provider
    liftIO $ skottie_animation_builder_set_resource_provider builder' (coerceToSkottieResourceProviderPtr provider')

setFontManager :: (MonadIO m) => SkottieAnimationBuilder -> SKFontManager -> m ()
setFontManager builder fmgr = evalContIO do
    builder' <- useObj builder
    fmgr' <- useObj fmgr
    liftIO $ skottie_animation_builder_set_font_manager builder' fmgr'

buildFromStream ::
    (MonadIO m, IsSKStream stream) =>
    SkottieAnimationBuilder ->
    stream ->
    m (Maybe (Ref SkottieAnimation))
buildFromStream builder (toA SKStream -> stream) = evalContIO do
    builder' <- useObj builder
    stream' <- useObj stream
    anim' <- liftIO $ skottie_animation_builder_make_from_stream builder' stream'
    toObjectFinUnlessNull skottie_animation_unref anim'

buildFromByteString ::
    (MonadIO m) =>
    SkottieAnimationBuilder ->
    BS.ByteString ->
    m (Maybe (Ref SkottieAnimation))
buildFromByteString builder bytestring = evalContIO do
    builder' <- useObj builder
    (cstr, len) <- ContT $ BS.unsafeUseAsCStringLen bytestring
    anim' <- liftIO $ skottie_animation_builder_make_from_data builder' cstr (fromIntegral len)
    toObjectFinUnlessNull skottie_animation_unref anim'

buildFromFile ::
    (MonadIO m) =>
    SkottieAnimationBuilder ->
    FilePath ->
    m (Maybe (Ref SkottieAnimation))
buildFromFile builder path = evalContIO do
    builder' <- useObj builder
    path' <- ContT $ Foreign.C.String.withCString path
    anim' <- liftIO $ skottie_animation_builder_make_from_file builder' path'
    toObjectFinUnlessNull skottie_animation_unref anim'