module Skia.SKRegionIterator where

import Skia.Internal.Prelude

delete :: (MonadIO m) => SKRegionIterator -> m ()
delete iter = evalContIO do
    iter' <- useObj iter
    liftIO $ sk_region_iterator_delete iter'

-- | Sets SkRegion::Iterator to return elements of SkIRect array in region.
create :: (MonadIO m) => SKRegion -> m SKRegionIterator
create region = evalContIO do
    region' <- useObj region
    iter' <- liftIO $ sk_region_iterator_new region'
    toObject iter'

{- | Point SkRegion::Iterator to start of SkRegion. Returns true if SkRegion was
set; otherwise, returns false.
-}
rewind :: (MonadIO m) => SKRegionIterator -> m Bool
rewind iter = evalContIO do
    iter' <- useObj iter

    r <- liftIO $ sk_region_iterator_rewind iter'
    pure $ toBool r

{- | Returns true if SkRegion::Iterator is pointing to final SkIRect in
SkRegion.
-}
isDone :: (MonadIO m) => SKRegionIterator -> m Bool
isDone iter = evalContIO do
    iter' <- useObj iter

    r <- liftIO $ sk_region_iterator_done iter'
    pure $ toBool r

-- | Advances SkRegion::Iterator to next SkIRect in SkRegion if it is not done.
next :: (MonadIO m) => SKRegionIterator -> m ()
next iter = evalContIO do
    iter' <- useObj iter
    liftIO $ sk_region_iterator_next iter'

{- | Returns SkIRect element in SkRegion. Does not return predictable results if
SkRegion is empty.
-}
getRect :: (MonadIO m) => SKRegionIterator -> m (Rect Int)
getRect iter = evalContIO do
    iter' <- useObj iter
    rect' <- useAlloca
    liftIO $ sk_region_iterator_rect iter' rect'
    liftIO $ fromSKIRect <$> peek rect'
