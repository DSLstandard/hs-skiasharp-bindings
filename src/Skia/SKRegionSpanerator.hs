module Skia.SKRegionSpanerator where

import Skia.Internal.Prelude

delete :: (MonadIO m) => SKRegionSpanerator -> m ()
delete iter = evalContIO do
    iter' <- useObj iter
    liftIO $ sk_region_spanerator_delete iter'

-- | Sets SkRegion::Spanerator to return line segments in SkRegion on scan line.
create ::
    (MonadIO m) =>
    SKRegion ->
    -- | y: Horizontal line to intersect
    Int32 ->
    -- | left: bounds of iteration
    Int32 ->
    -- | right: bounds of iteration
    Int32 ->
    m SKRegionSpanerator
create region y left right = evalContIO do
    region' <- useObj region
    iter' <- liftIO $ sk_region_spanerator_new region' (coerce y) (coerce left) (coerce right)
    toObject iter'

{- | Advances iterator to next span intersecting SkRegion within line segment
provided in constructor. Returns 'Nothing' if interval was not found.
-}
next :: (MonadIO m) => SKRegionSpanerator -> m (Maybe (Int32, Int32))
next iter = evalContIO do
    iter' <- useObj iter

    left' <- useAlloca
    right' <- useAlloca

    found <- liftIO $ sk_region_spanerator_next iter' left' right'
    if toBool found
        then do
            left <- liftIO $ peek left'
            right <- liftIO $ peek right'
            pure $ Just (coerce left, coerce right)
        else do
            pure Nothing
