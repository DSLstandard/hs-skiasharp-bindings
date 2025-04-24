module Main where

import Control.Monad
import Foreign
import Foreign.C
import SkiaSharp.Bindings

main :: IO ()
main = do
    let outputPngFilePath = "raster-example-output.png"

    -- Example somewhat follows the Raster code from
    -- https://skia.org/docs/user/api/skcanvas_creation/.
    colorspace <- skColorSpaceNewSrgb
    let
        iminfo =
            SkImageInfo
                { colorspace = colorspace
                , width = 800
                , height = 800
                , colorType = skColorType'Argb4444
                , alphaType = skAlphaType'Premul
                }

    surf <- with iminfo \iminfo -> do
        -- NOTE: if input rowBytes = 0, skia auto-computes rowBytes.
        skSurfaceNewRaster iminfo 0 nullPtr

    canvas <- skSurfaceGetCanvas surf
    skCanvasClearColor4f canvas (SkColor4F 0.0 1.0 1.0 1.0)

    paint <- skPaintNew
    skPaintSetColor paint 0xFF0000FF
    skCanvasDrawCircle canvas 400 400 200 paint
    skPaintDelete paint

    image <- skSurfaceNewImageSnapshot surf
    when (image == nullPtr) $ error "bad image"

    fileStream <- withCString outputPngFilePath \path -> do
        skFileWStreamNew path

    -- Logic is derived from SkiaSharp's binding/SkiaSharp/SKImage.cs's SKImage::Encode(format, quality).
    rasterImage <- skImageMakeRasterImage image
    pixmap <- skPixmapNew
    ok <- skImagePeekPixels rasterImage pixmap
    when (ok == 0) $ error "skImageReadPixelsIntoPixmap failed"

    ok <- with defaultSkPngEncoderOptions \pngEncoderOptions -> do
        skPngEncoderEncode (castPtr fileStream) pixmap pngEncoderOptions
    when (ok == 0) $ error "skPngEncoderEncode failed"

    skFileWStreamDestroy fileStream
    skImageUnRef rasterImage
    skImageUnRef image
    skPixmapDestructOr pixmap
    skSurfaceUnRef surf
    skColorSpaceUnRef colorspace

    putStrLn $ "[!] Saved image to '" <> outputPngFilePath <> "'"