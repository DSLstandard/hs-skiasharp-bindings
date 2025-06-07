module Skia.SKColorType where

import Skia.Internal.Prelude

{- | Selects the native 32-bit ARGB format for the current configuration. This
can lead to inconsistent results across platforms, so use with caution.
-}
getNativeArgb32Format :: (MonadIO m) => m SKColorType
getNativeArgb32Format = liftIO do
    ty <- liftIO $ sk_colortype_get_default_8888
    unmarshalSKEnumOrDie ty

{- DEVELOPER NOTE:

All the mappings came from Google Skia's src/core/SkImageInfo.cpp and
binding/SkiaSharp/Definitions.cs
-}

{- | Returns the number of bytes per pixel.

Note that 'SKColorType\'Unknown' returns 0.
-}
bytesPerPixel :: SKColorType -> Int
bytesPerPixel = \case {}

{-
    -- Unknown
    SKColorType'Unknown -> 0
    -- 1
    SKColorType'Alpha8 -> 1
    SKColorType'Gray8 -> 1
    SKColorType'R8Unorm -> 1
    -- 2
    SKColorType'Rgb565 -> 2
    SKColorType'Argb4444 -> 2
    SKColorType'Rg88 -> 2
    SKColorType'Alpha16Unorm -> 2
    SKColorType'Alpha16Float -> 2
    -- TODO: FIXME: 2
    -- 4
    SKColorType'Bgra8888 -> 4
    SKColorType'Bgra1010102 -> 4
    SKColorType'Bgr101010x -> 4
    SKColorType'Bgr101010xXR -> 4
    SKColorType'Rgba8888 -> 4
    SKColorType'Rgb888x -> 4
    SKColorType'Rgba1010102 -> 4
    SKColorType'Rgb101010x -> 4
    SKColorType'Rg1616 -> 4
    SKColorType'RgF16 -> 4
    SKColorType'Srgba8888 -> 4
    -- 8
    SKColorType'RgbaF16Clamped -> 8
    SKColorType'RgbaF16 -> 8
    SKColorType'Rgba16161616 -> 8
    SKColorType'Rgba10x6 -> 8
    -- 16
    SKColorType'RgbaF32 -> 16

{- | Returns the bit shift per pixel.

Note that 'SKColorType\'Unknown' returns 0.
-}
bitShiftPerPixel :: SKColorType -> Int
bitShiftPerPixel = \case
    -- Unknown
    SKColorType'Unknown -> 0
    -- 0
    SKColorType'Alpha8 -> 0
    SKColorType'Gray8 -> 0
    SKColorType'R8Unorm -> 0
    -- 1
    SKColorType'Rgb565 -> 1
    SKColorType'Argb4444 -> 1
    SKColorType'Rg88 -> 1
    SKColorType'Alpha16 -> 1
    SKColorType'A16Float -> 1
    -- 2
    SKColorType'Bgra8888 -> 2
    SKColorType'Bgra1010102 -> 2
    SKColorType'Bgr101010x -> 2
    SKColorType'Bgr101010xXR -> 2
    SKColorType'Rgba8888 -> 2
    SKColorType'Rgb888x -> 2
    SKColorType'Rgba1010102 -> 2
    SKColorType'Rgb101010x -> 2
    SKColorType'Rg1616 -> 2
    SKColorType'RgF16 -> 2
    SKColorType'Srgba8888 -> 2
    -- 3
    SKColorType'RgbaF16Clamped -> 3
    SKColorType'RgbaF16 -> 3
    SKColorType'Rgba16161616 -> 3
    SKColorType'Rgba10x6 -> 3
    -- 4
    SKColorType'RgbaF32 -> 4

validateAlphaType :: SKColorType -> SKAlphaType -> SKAlphaType
validateAlphaType colorType alphaType =
    -- FIXME: 1 clause, multiple constructors like Rust?
    case colorType of
        SKColorType'Unknown -> SKAlphaType'Unknown
        -- SkiaSharp: "Opaque or premul"
        SKColorType'Alpha8 -> onOpaqueOrPremul
        SKColorType'Alpha16 -> onOpaqueOrPremul
        SKColorType'AlphaF16 -> onOpaqueOrPremul
        -- SkiaSharp: "Any"
        SKColorType'ARGB4444 -> onAny
        SKColorType'RGBA8888 -> onAny
        SKColorType'BGRA8888 -> onAny
        SKColorType'SRGBA8888 -> onAny
        SKColorType'RGBA1010102 -> onAny
        SKColorType'BGRA1010102 -> onAny
        SKColorType'RGBAF16Clamped -> onAny
        SKColorType'RGBAF16 -> onAny
        SKColorType'RGBAF32 -> onAny
        SKColorType'RGBA16161616 -> onAny
        SKColorType'RGBA10x6 -> onAny
        -- SkiaSharp: "Opaque"
		SKColorType'Gray8 -> onOpaque
		SKColorType'R88 -> onOpaque
		SKColorType'RG1616 -> onOpaque
		SKColorType'RGF16 -> onOpaque
		SKColorType'RGB565 -> onOpaque
		SKColorType'RGB888x -> onOpaque
		SKColorType'RGB101010x -> onOpaque
		SKColorType'BGR101010x -> onOpaque
		SKColorType'BGR101010xXr -> onOpaque
		SKColorType'R8Unorm -> onOpaque
  where
    onOpaqueOrPremul = case alphaType of
        SKAlphaType'Unpremul -> SKAlphaType'Premul
        _ -> alphaType

    onAny = alphaType

    onOpaque = SKAlphaType'Opaque
-}
