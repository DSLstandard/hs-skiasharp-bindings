module Skia.SkottieAnimationBuilder where

import Control.Monad.Trans.Resource
import Data.Acquire
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

create ::
    -- | Additional options. Consider using 'defaultCreateFlags'.
    CreateFlags ->
    Acquire SkottieAnimationBuilder
create flags =
    mkSKObjectAcquire
        (skottie_animation_builder_new (marshalCreateFlags flags))
        skottie_animation_builder_delete

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
    (MonadResource m, IsSKStream stream) =>
    SkottieAnimationBuilder ->
    stream ->
    m (Maybe (ReleaseKey, SkottieAnimation))
buildFromStream builder (toA SKStream -> stream) =
    allocateSKObjectUnlessNull
        (skottie_animation_builder_make_from_stream (ptr builder) (ptr stream))
        skottie_animation_unref

buildFromByteString ::
    (MonadResource m) =>
    SkottieAnimationBuilder ->
    BS.ByteString ->
    m (Maybe (ReleaseKey, SkottieAnimation))
buildFromByteString builder bytestring =
    allocateSKObjectUnlessNull
        ( evalContIO do
            (cstr, len) <- ContT $ BS.unsafeUseAsCStringLen bytestring
            liftIO $ skottie_animation_builder_make_from_data (ptr builder) cstr (fromIntegral len)
        )
        skottie_animation_unref

buildFromFile ::
    (MonadResource m) =>
    SkottieAnimationBuilder ->
    FilePath ->
    m (Maybe (ReleaseKey, SkottieAnimation))
buildFromFile builder path =
    allocateSKObjectUnlessNull
        ( evalContIO do
            path' <- ContT $ Foreign.C.String.withCString path
            liftIO $ skottie_animation_builder_make_from_file (ptr builder) path'
        )
        skottie_animation_unref
