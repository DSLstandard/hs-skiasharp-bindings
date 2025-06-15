module Skia.SKVertices where

import Control.Exception
import Control.Monad.Trans.Resource
import Data.Vector.Storable qualified as VS
import Skia.Internal.Prelude

-- | Create a vertices by copying the specified arrays.
createCopy ::
    (MonadResource m) =>
    SKVerticesVertexMode ->
    -- | positions. This determines the number of vertices.
    VS.Vector Sk_point ->
    -- | texs. Optional. If 'Just', the length must be the same as that of positions.
    Maybe (VS.Vector Sk_point) ->
    -- | colors. Optional. If 'Just', the length must be the same as that of positions.
    Maybe (VS.Vector SKColor) ->
    -- | Indices. Optional.
    Maybe (VS.Vector Word16) ->
    m (ReleaseKey, SKVertices)
createCopy mode positions texs colors indices =
    allocateSKObject
        ( evalContIO do
            let vertexCount = VS.length positions

            positions' <- ContT $ VS.unsafeWith positions

            let
                -- For texs and colors
                useArray :: (Storable a) => String -> Maybe (VS.Vector a) -> ContT r IO (Ptr a)
                useArray _name Nothing = pure nullPtr
                useArray name (Just array) = do
                    when (VS.length array /= vertexCount) do
                        let err =
                                BadArgumentError $
                                    "length of "
                                        <> name
                                        <> " (="
                                        <> show (VS.length array)
                                        <> ") must match that of positions (="
                                        <> show vertexCount
                                        <> ")"
                        liftIO $ throwIO err

                    ContT $ VS.unsafeWith array

            texs' <- useArray "texs" texs
            colors' <- useArray "colors" colors

            {-
                Google Skia's note on indices:

                /**
                *  Create a vertices by copying the specified arrays. texs, colors may be nullptr,
                *  and indices is ignored if indexCount == 0.
                */
            -}

            let indexCount = maybe 0 VS.length indices
            indices' <- case indices of
                Nothing -> pure nullPtr -- use nullPtr as dummy value
                Just indices -> ContT $ VS.unsafeWith indices

            liftIO $
                sk_vertices_make_copy
                    (marshalSKEnum mode)
                    (fromIntegral vertexCount)
                    positions'
                    texs'
                    (coercePtr colors')
                    (fromIntegral indexCount)
                    indices'
        )
        sk_vertices_unref
