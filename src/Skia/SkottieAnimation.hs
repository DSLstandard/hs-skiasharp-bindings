{- | You may want to these links to learn more about Lottie and Skottie
animations:

* <https://lottiefiles.com/what-is-lottie>

* <https://skia.org/docs/user/modules/skottie/>
-}
module Skia.SkottieAnimation where

import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BS
import Data.Text qualified as T
import Foreign.C.String qualified
import Linear
import Skia.Internal.Prelude
import Skia.SKString qualified as SKString

-- | Returns the animation frame rate (frames / second).
getFPS :: (MonadIO m) => SkottieAnimation -> m Double
getFPS anim = evalContIO do
    anim' <- useObj anim
    liftIO $ fmap coerce $ skottie_animation_get_fps anim'

-- | Returns the dimensions of the animation.
getSize :: (MonadIO m) => SkottieAnimation -> m (V2 Float)
getSize anim = evalContIO do
    anim' <- useObj anim
    size' <- useAlloca
    liftIO $ skottie_animation_get_size anim' size'
    peekWith fromSKSize size'

-- | Returns the animation duration in seconds.
getDuration :: (MonadIO m) => SkottieAnimation -> m Double
getDuration anim = evalContIO do
    anim' <- useObj anim
    liftIO $ fmap coerce $ skottie_animation_get_duration anim'

-- | Animation in point, in frame index units.
getInPoint :: (MonadIO m) => SkottieAnimation -> m Double
getInPoint anim = evalContIO do
    anim' <- useObj anim
    liftIO $ fmap coerce $ skottie_animation_get_in_point anim'

-- | Animation out point, in frame index units.
getOutPoint :: (MonadIO m) => SkottieAnimation -> m Double
getOutPoint anim = evalContIO do
    anim' <- useObj anim
    liftIO $ fmap coerce $ skottie_animation_get_out_point anim'

getVersion :: (MonadIO m) => SkottieAnimation -> m T.Text
getVersion anim = evalContIO do
    anim' <- useObj anim

    version <- SKString.createEmpty
    version' <- useObj version

    liftIO $ skottie_animation_get_version anim' version'

    versionTxt <- SKString.getAsText version
    disposeObject version

    pure versionTxt

data RenderFlags = RenderFlags
    { skipTopLevelIsolation :: Bool
    , disableTopLevelClipping :: Bool
    }
    deriving (Show, Eq, Ord)

defaultRenderFlags :: RenderFlags
defaultRenderFlags =
    RenderFlags
        { skipTopLevelIsolation = False
        , disableTopLevelClipping = False
        }

marshalRenderFlags :: RenderFlags -> Skottie_animation_renderflags
marshalRenderFlags flags =
    makeBitFlags
        [ (flags.skipTopLevelIsolation, SKIP_TOP_LEVEL_ISOLATION)
        , (flags.disableTopLevelClipping, DISABLE_TOP_LEVEL_CLIPPING)
        ]

{- | Draws the current animation frame.

It is undefined behavior to call 'render'' on a newly created 'SkottieAnimation'
before specifying an initial frame via 'seek' or its variants.
-}
render ::
    (MonadIO m, IsSKCanvas canvas) =>
    SkottieAnimation ->
    -- | Destination canvas
    canvas ->
    -- | Optional draw destination rect
    Maybe (Rect Float) ->
    -- | Render flags. Consider using 'defaultRenderFlags'.
    RenderFlags ->
    m ()
render anim (toA SKCanvas -> dstCanvas) dstRect flags = evalContIO do
    anim' <- useObj anim
    dstCanvas' <- useObj dstCanvas
    dstRect' <- useNullIfNothing useStorable $ fmap toSKRect $ dstRect
    liftIO $ skottie_animation_render_with_flags anim' dstCanvas' dstRect' (marshalRenderFlags flags)

{-# DEPRECATED seek "Google Skia recommends \"us[ing] one of the other versions\" of 'seek'." #-}

-- | Updates the animation state for @t@.
seek ::
    (MonadIO m) =>
    SkottieAnimation ->
    -- | @t@: normalized [0..1] frame selector (0 -> first frame, 1 -> final frame)
    Float ->
    -- | Optional invalidation controller (dirty region tracking)
    Maybe SKSGInvalidationController ->
    m ()
seek anim t ic = evalContIO do
    anim' <- useObj anim
    ic' <- useNullIfNothing useObj ic
    liftIO $ skottie_animation_seek anim' (coerce t) ic'

{- |
Update the animation state to match @t@, specified as a frame index i.e.
relative to @'getDuration' * 'getFPS'@.

Fractional values are allowed and meaningful - e.g.

  * 0.0 -> first frame

  * 1.0 -> second frame

  * 0.5 -> halfway between first and second frame
-}
seekFrame ::
    (MonadIO m) =>
    SkottieAnimation ->
    -- | Frame index. Should be between [0..'getDuration' * 'getFPS')
    Float ->
    -- | Optional invalidation controller (dirty region tracking)
    Maybe SKSGInvalidationController ->
    m ()
seekFrame anim t ic = evalContIO do
    anim' <- useObj anim
    ic' <- useNullIfNothing useObj ic
    liftIO $ skottie_animation_seek_frame anim' (coerce t) ic'

{- | Update the animation state to match @t@, specified in frame time i.e.
relative to 'getDuration'.
-}
seekFrameTime ::
    (MonadIO m) =>
    SkottieAnimation ->
    -- | Frame time. Should be between [0..'getDuration' * 'getFPS')
    Float ->
    -- | Optional invalidation controller (dirty region tracking)
    Maybe SKSGInvalidationController ->
    m ()
seekFrameTime anim t ic = evalContIO do
    anim' <- useObj anim
    ic' <- useNullIfNothing useObj ic
    liftIO $ skottie_animation_seek_frame_time anim' (coerce t) ic'