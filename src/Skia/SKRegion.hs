module Skia.SKRegion where

import Linear
import Skia.Internal.Prelude
import Skia.SKPath qualified as SKPath
import Skia.Types.Rect qualified as Rect

delete :: (MonadIO m) => SKRegion -> m ()
delete region = evalContIO do
    region' <- useObj region
    liftIO $ sk_region_delete region'

create :: (MonadIO m) => m SKRegion
create = evalContIO do
    region' <- liftIO sk_region_new
    toObject region'

isEmpty :: (MonadIO m) => SKRegion -> m Bool
isEmpty region = evalContIO do
    region' <- useObj region
    r <- liftIO $ sk_region_is_empty region'
    pure $ toBool r

isRect :: (MonadIO m) => SKRegion -> m Bool
isRect region = evalContIO do
    region' <- useObj region
    r <- liftIO $ sk_region_is_rect region'
    pure $ toBool r

isComplex :: (MonadIO m) => SKRegion -> m Bool
isComplex region = evalContIO do
    region' <- useObj region
    r <- liftIO $ sk_region_is_complex region'
    pure $ toBool r

{- | Constructs an empty SkRegion. SkRegion is set to empty bounds at (0, 0)
with zero width and height.
-}
setEmpty :: (MonadIO m) => SKRegion -> m ()
setEmpty region = evalContIO do
    region' <- useObj region

    -- Google's commnet says:
    --
    -- Constructs an empty SkRegion. SkRegion is set to empty bounds at (0, 0)
    -- with zero width and height. ***Always returns false.***
    void $ liftIO $ sk_region_set_empty region'

{- | Constructs a rectangular SkRegion matching the bounds of rect. If rect is
empty, constructs empty and returns false.
-}
setRect :: (MonadIO m) => SKRegion -> Rect Int -> m Bool
setRect region rect = evalContIO do
    region' <- useObj region
    rect' <- useStorable $ toSKIRect rect

    r <- liftIO $ sk_region_set_rect region' rect'
    pure $ toBool r

{- | Constructs SkRegion as the union of SkIRect in rects array. If count is
zero, constructs empty SkRegion. Returns false if constructed SkRegion is empty.

May be faster than repeated calls to op().
-}
setRectsRaw ::
    (MonadIO m) =>
    SKRegion ->
    -- | Pointer to an array of rectangles.
    Ptr Sk_irect ->
    -- | Number of rectangles.
    Int ->
    m Bool
setRectsRaw region rects' count = evalContIO do
    region' <- useObj region

    r <- liftIO $ sk_region_set_rects region' rects' (fromIntegral count)
    pure $ toBool r

{- | Constructs SkRegion to match outline of path within clip. Returns false if
constructed SkRegion is empty.

Constructed SkRegion draws the same pixels as path through clip when
anti-aliasing is disabled.
-}
setRegion :: (MonadIO m) => SKRegion -> SKRegion -> m Bool
setRegion region other = evalContIO do
    region' <- useObj region
    other' <- useObj other
    r <- liftIO $ sk_region_set_region region' other'
    pure $ toBool r

{- | Constructs SkRegion to match outline of path within clip. Returns false if
constructed SkRegion is empty.

Constructed SkRegion draws the same pixels as path through clip when
anti-aliasing is disabled.
-}
setPath ::
    (MonadIO m) =>
    SKRegion ->
    SKPath ->
    -- | Clip
    SKRegion ->
    m Bool
setPath region path clip = evalContIO do
    region' <- useObj region
    path' <- useObj path
    clip' <- useObj clip

    r <- liftIO $ sk_region_set_path region' path' clip'
    pure $ toBool r

{- | Gets the bounds of this region.

If the region is empty, returns an empty rectangle.
-}
getBounds :: (MonadIO m) => SKRegion -> m (Maybe (Rect Int))
getBounds region = evalContIO do
    region' <- useObj region
    bounds' <- useAlloca
    liftIO $ sk_region_get_bounds region' bounds'

    bounds <- liftIO $ fromSKIRect <$> peek bounds'
    pure $ if Rect.isEmpty bounds then Nothing else Just bounds

containsPoint :: (MonadIO m) => SKRegion -> V2 Int -> m Bool
containsPoint region (V2 x y) = evalContIO do
    region' <- useObj region
    r <- liftIO $ sk_region_contains_point region' (fromIntegral x) (fromIntegral y)
    pure $ toBool r

containsRect :: (MonadIO m) => SKRegion -> Rect Int -> m Bool
containsRect region rect = evalContIO do
    region' <- useObj region
    rect' <- useStorable $ toSKIRect rect
    r <- liftIO $ sk_region_contains_rect region' rect'
    pure $ toBool r

containsRegion :: (MonadIO m) => SKRegion -> SKRegion -> m Bool
containsRegion region other = evalContIO do
    region' <- useObj region
    other' <- useObj other
    r <- liftIO $ sk_region_contains region' other'
    pure $ toBool r

{- | Returns true if SkRegion is a single rectangle and contains r. May return
false even though SkRegion contains r.
-}
quickContainsRect :: (MonadIO m) => SKRegion -> Rect Int -> m Bool
quickContainsRect region rect = evalContIO do
    region' <- useObj region
    rect' <- useStorable $ toSKIRect rect
    r <- liftIO $ sk_region_quick_contains region' rect'
    pure $ toBool r

{- | Returns true if SkRegion does not intersect rect. Returns true if rect is
empty or SkRegion is empty. May return false even though SkRegion does not
intersect rect.
-}
quickRejectRect :: (MonadIO m) => SKRegion -> Rect Int -> m Bool
quickRejectRect region rect = evalContIO do
    region' <- useObj region
    rect' <- useStorable $ toSKIRect rect
    r <- liftIO $ sk_region_quick_reject_rect region' rect'
    pure $ toBool r

{- | Returns true if SkRegion does not intersect rgn. Returns true if rgn is
empty or SkRegion is empty. May return false even though SkRegion does not
intersect rgn.
-}
quickRejectRegion :: (MonadIO m) => SKRegion -> SKRegion -> m Bool
quickRejectRegion region other = evalContIO do
    region' <- useObj region
    other' <- useObj other
    r <- liftIO $ sk_region_quick_reject region' other'
    pure $ toBool r

translate :: (MonadIO m) => SKRegion -> V2 Int -> m ()
translate region (V2 dx dy) = evalContIO do
    region' <- useObj region
    liftIO $ sk_region_translate region' (fromIntegral dx) (fromIntegral dy)

{- | Set this region to the result of applying the operation to this region and
the specified rectangle.

Returns true if the resulting region is non-empty.
-}
opRect :: (MonadIO m) => SKRegion -> Rect Int -> SKRegionOp -> m Bool
opRect region rect operation = evalContIO do
    region' <- useObj region
    rect' <- useStorable $ toSKIRect rect

    r <- liftIO $ sk_region_op_rect region' rect' (marshalSKEnum operation)
    pure $ toBool r

{- | Set this region to the result of applying the operation to this region and
the specified region.

Returns true if the resulting region is non-empty.
-}
opRegion :: (MonadIO m) => SKRegion -> SKRegion -> SKRegionOp -> m Bool
opRegion region other operation = evalContIO do
    region' <- useObj region
    other' <- useObj other

    r <- liftIO $ sk_region_op region' other' (marshalSKEnum operation)
    pure $ toBool r

{- | Returns true if SkRegion intersects rect. Returns false if either rect or
SkRegion is empty, or do not intersect.
-}
intersectsRect :: (MonadIO m) => SKRegion -> Rect Int -> m Bool
intersectsRect region rect = evalContIO do
    region' <- useObj region
    rect' <- useStorable $ toSKIRect rect

    r <- liftIO $ sk_region_intersects_rect region' rect'
    pure $ toBool r

{- | Returns true if SkRegion intersects other.
Returns false if either other or SkRegion is empty, or do not intersect.
-}
intersectsRegion :: (MonadIO m) => SKRegion -> SKRegion -> m Bool
intersectsRegion region other = evalContIO do
    region' <- useObj region
    other' <- useObj other

    r <- liftIO $ sk_region_intersects region' other'
    pure $ toBool r

{- | Appends outline of SkRegion to path. Returns true if SkRegion is not empty;
otherwise, returns false, and leaves path unmodified.
-}
getBoundaryPathToDest ::
    (MonadIO m) =>
    SKRegion ->
    -- | Destination path
    SKPath ->
    m Bool
getBoundaryPathToDest region path = evalContIO do
    region' <- useObj region
    path' <- useObj path

    r <- liftIO $ sk_region_get_boundary_path region' path'
    pure $ toBool r

{- | Like 'getBoundaryPathToDest' but creates the path for you and returns it.
Returns 'Nothing' if the result path is empty.
-}
getBoundaryPath :: (MonadIO m) => SKRegion -> m (Maybe SKPath)
getBoundaryPath region = do
    path <- SKPath.create
    isEmpty <- getBoundaryPathToDest region path
    if isEmpty
        then do
            disposeObject path
            pure Nothing
        else do
            pure $ Just path
