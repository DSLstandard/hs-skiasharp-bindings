module Skia.SKColor where

import Skia.Internal.Prelude
import Skia.Types.Color

-- DEVNOTE: This is called 'convertRGBA2BGRA' because of Google Skia's
-- src/core/SkSwizzle.cpp.

-- | Efficiently converts an array of RGBA to BGRA (or vice versa).
convertRgbaToBgraArray ::
    (MonadIO m) =>
    -- | Destination BGRA array.
    Ptr SKColor ->
    -- | Source RGBA array
    Ptr SKColor ->
    -- | Array length
    Int ->
    m ()
convertRgbaToBgraArray dst src len = liftIO do
    sk_swizzle_swap_rb (coercePtr dst) (coercePtr src) (fromIntegral len)

-- | Efficiently premultiplies an array of 'SKColor'.
premultiplyArray ::
    (MonadIO m) =>
    -- | Destination 'SKPMColor'
    Ptr SKPMColor ->
    -- | Source 'SKColor'
    Ptr SKColor ->
    -- | Array length
    Int ->
    m ()
premultiplyArray dst src len = liftIO do
    sk_color_premultiply_array (coercePtr src) (fromIntegral len) (coercePtr dst)

-- | Efficiently unpremultiplies an array of 'SKPMColor'.
unpremultiplyArray ::
    (MonadIO m) =>
    -- | Destination 'SKColor'
    Ptr SKColor ->
    -- | Source 'SKPMColor'
    Ptr SKPMColor ->
    -- | Array length
    Int ->
    m ()
unpremultiplyArray dst src len = liftIO do
    sk_color_unpremultiply_array (coercePtr src) (fromIntegral len) (coercePtr dst)
