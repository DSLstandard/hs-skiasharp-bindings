module Skia.SKVertices where

import Skia.Internal.Prelude

createCopyRaw ::
    (MonadIO m) =>
    SKVerticesVertexMode ->
    -- | Vertex count
    Int ->
    -- | Positions
    Ptr Sk_point ->
    -- | Texs
    Ptr Sk_point ->
    -- | Colors
    Ptr SKColor ->
    -- | Index count
    Int ->
    -- | Indices
    Ptr Word16 ->
    m SKVertices
createCopyRaw mode vertexCount positions texs colors indexCount indices = liftIO $ do
    vertices' <-
        sk_vertices_make_copy
            (marshalSKEnum mode)
            (fromIntegral vertexCount)
            positions
            texs
            (coercePtr colors)
            (fromIntegral indexCount)
            indices
    toObjectFin sk_vertices_unref vertices'
