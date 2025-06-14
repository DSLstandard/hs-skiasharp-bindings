module Skia.SKRegionCliperator where

import Skia.Internal.Prelude

delete :: (MonadIO m) => SKRegionCliperator -> m ()
delete iter = evalContIO do
    iter' <- useObj iter
    liftIO $ sk_region_cliperator_delete iter'

-- | Sets SkRegion::Cliperator to return elements of SkIRect array in SkRegion within clip.
create ::
    (MonadIO m) =>
    SKRegion ->
    -- | Clip
    Rect Int ->
    m SKRegionCliperator
create region clip = evalContIO do
    region' <- useObj region
    clip' <- useStorable $ toSKIRect clip

    iter' <- liftIO $ sk_region_cliperator_new region' clip'
    toObject iter'

-- | Returns true if SkRegion::Cliperator is pointing to final SkIRect in SkRegion.
isDone :: (MonadIO m) => SKRegionCliperator -> m Bool
isDone iter = evalContIO do
    iter' <- useObj iter

    r <- liftIO $ sk_region_cliperator_done iter'
    pure $ toBool r

-- | Advances iterator to next SkIRect in SkRegion contained by clip.
next :: (MonadIO m) => SKRegionCliperator -> m ()
next iter = evalContIO do
    iter' <- useObj iter
    liftIO $ sk_region_cliperator_next iter'

{- | Returns SkIRect element in SkRegion, intersected with clip passed to
SkRegion::Cliperator constructor. Does not return predictable results if SkRegion
is empty.
-}
getRect :: (MonadIO m) => SKRegionCliperator -> m (Rect Int)
getRect iter = evalContIO do
    iter' <- useObj iter
    rect' <- useAlloca
    liftIO $ sk_region_cliperator_rect iter' rect'
    liftIO $ fromSKIRect <$> peek rect'
