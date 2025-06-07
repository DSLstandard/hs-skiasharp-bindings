module Skia.SKOverdrawCanvas where

import Skia.Internal.Prelude

destroy :: (MonadIO m) => SKOverdrawCanvas -> m ()
destroy canvas = evalContIO do
    canvas' <- useObj canvas
    liftIO $ sk_overdraw_canvas_destroy canvas'

create ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    m SKOverdrawCanvas
create (toA SKCanvas -> canvas) = evalContIO do
    canvas' <- useObj canvas
    overdrawcanvas' <- liftIO $ sk_overdraw_canvas_new canvas'
    toObject overdrawcanvas'
