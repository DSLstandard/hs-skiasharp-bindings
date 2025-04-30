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
    colorspace <- sk_colorspace_new_srgb
    let
        iminfo =
            Sk_imageinfo
                { colorspace = colorspace
                , width = 800
                , height = 800
                , colorType = sk_colortype'ARGB_4444_SK_COLORTYPE
                , alphaType = sk_alphatype'PREMUL_SK_ALPHATYPE
                }

    surf <- with iminfo \iminfo -> do
        -- NOTE: if input rowBytes = 0, skia auto-computes rowBytes.
        sk_surface_new_raster iminfo 0 nullPtr

    canvas <- sk_surface_get_canvas surf
    sk_canvas_clear_color4f canvas (Sk_color4f 0.0 1.0 1.0 1.0)

    paint <- sk_paint_new
    sk_paint_set_color paint 0xFF0000FF
    sk_canvas_draw_circle canvas 400 400 200 paint
    sk_paint_delete paint

    image <- sk_surface_new_image_snapshot surf
    when (image == nullPtr) $ error "bad image"

    fileStream <- withCString outputPngFilePath \path -> do
        sk_filewstream_new path

    -- Logic is derived from SkiaSharp's binding/SkiaSharp/SKImage.cs's SKImage::Encode(format, quality).
    rasterImage <- sk_image_make_raster_image image
    pixmap <- sk_pixmap_new
    ok <- sk_image_peek_pixels rasterImage pixmap
    when (ok == 0) $ error "skImageReadPixelsIntoPixmap failed"

    ok <- with defaultSkPngEncoderOptions \pngEncoderOptions -> do
        sk_pngencoder_encode (castPtr fileStream) pixmap pngEncoderOptions
    when (ok == 0) $ error "skPngEncoderEncode failed"

    sk_filewstream_destroy fileStream
    sk_image_unref rasterImage
    sk_image_unref image
    sk_pixmap_destructor pixmap
    sk_surface_unref surf
    sk_colorspace_unref colorspace

    putStrLn $ "[!] Saved image to '" <> outputPngFilePath <> "'"