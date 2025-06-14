module Skia.SKSvg where

import Skia.Internal.Prelude

{- | Returns a new canvas that will generate SVG commands from its draw calls,
and send them to the provided stream. Ownership of the stream is not
transfered, and it must remain valid for the lifetime of the returned canvas.

The canvas may buffer some drawing calls, so the output is not guaranteed to
be valid or complete until the canvas instance is deleted.

The \"bounds\" parameter defines an initial SVG viewport (viewBox attribute on
the root SVG element).

Returns 'Nothing' if the operation fails.
-}
createWithStream ::
    ( MonadIO m
    , IsSKWStream stream
    ) =>
    -- | \"bounds\"
    Rect Float ->
    stream ->
    m (Maybe (Owned SKCanvas))
createWithStream bounds (toA SKWStream -> stream) = evalContIO do
    stream' <- useObj stream
    bounds' <- useStorable $ toSKRect bounds
    canvas' <- liftIO $ sk_svgcanvas_create_with_stream bounds' stream'
    toObjectFinUnlessNull sk_canvas_destroy canvas'
