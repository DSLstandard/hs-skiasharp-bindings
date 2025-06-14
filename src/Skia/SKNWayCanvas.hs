module Skia.SKNWayCanvas where

import Skia.Internal.Prelude

destroy :: (MonadIO m) => SKNWayCanvas -> m ()
destroy canvas = evalContIO do
    canvas' <- useObj canvas
    liftIO $ sk_nway_canvas_destroy canvas'

create ::
    (MonadIO m) =>
    -- | Width
    Int ->
    -- | Height
    Int ->
    m SKNWayCanvas
create width height = liftIO do
    canvas' <- sk_nway_canvas_new (fromIntegral width) (fromIntegral height)
    toObject canvas'

addCanvas :: (MonadIO m, IsSKNWayCanvas nwaycanvas, IsSKCanvas canvas) => nwaycanvas -> canvas -> m ()
addCanvas (toA SKNWayCanvas -> nwaycanvas) (toA SKCanvas -> canvas) = evalContIO do
    canvas' <- useObj canvas
    nwaycanvas' <- useObj nwaycanvas
    liftIO $ sk_nway_canvas_add_canvas nwaycanvas' canvas'

removeCanvas :: (MonadIO m, IsSKNWayCanvas nwaycanvas, IsSKCanvas canvas) => nwaycanvas -> canvas -> m ()
removeCanvas (toA SKNWayCanvas -> nwaycanvas) (toA SKCanvas -> canvas) = evalContIO do
    canvas' <- useObj canvas
    nwaycanvas' <- useObj nwaycanvas
    liftIO $ sk_nway_canvas_remove_canvas nwaycanvas' canvas'

removeAllCanvases :: (MonadIO m, IsSKNWayCanvas nwaycanvas) => nwaycanvas -> m ()
removeAllCanvases (toA SKNWayCanvas -> nwaycanvas) = evalContIO do
    nwaycanvas' <- useObj nwaycanvas
    liftIO $ sk_nway_canvas_remove_all nwaycanvas'
