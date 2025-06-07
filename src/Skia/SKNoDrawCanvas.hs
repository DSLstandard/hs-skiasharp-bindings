module Skia.SKNoDrawCanvas where

import Skia.Internal.Prelude

destroy :: (MonadIO m) => SKNoDrawCanvas -> m ()
destroy canvas = evalContIO do
    canvas' <- useObj canvas
    liftIO $ sk_nodraw_canvas_destroy canvas'

create ::
    (MonadIO m) =>
    -- | Width
    Int ->
    -- | Height
    Int ->
    m SKNoDrawCanvas
create width height = liftIO do
    canvas' <- sk_nodraw_canvas_new (fromIntegral width) (fromIntegral height)
    toObject canvas'
