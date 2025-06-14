module Main where

import Control.Monad
import Control.Monad.Fix
import Foreign.C
import Graphics.GL qualified as GL
import Graphics.UI.GLFW qualified as GLFW
import Skia.Bindings
import Skia.GRBackendRenderTarget qualified as GRBackendRenderTarget
import Skia.GRDirectContext qualified as GRDirectContext
import Skia.GRGlInterface qualified as GRGlInterface
import Skia.SKCanvas qualified as SKCanvas
import Skia.SKPaint qualified as SKPaint
import Skia.SKSurface qualified as SKSurface
import Skia.Types (
    GRDirectContext,
    GRSurfaceOrigin (GRSurfaceOrigin'BottomLeft),
    RGBA (RGBA),
    Rect (Rect, bottom, left, right, top),
    SKColorType (SKColorType'RGBA'8888),
    SKSurface,
 )
import System.Exit
import System.IO

{-
Demo of setting up Skia on GLFW with an OpenGL backend.

This demo's implementation is translated from Kevin Yin
(<https://github.com/ad8e>)'s Github GIST here:
https://gist.github.com/ad8e/dd150b775ae6aa4d5cf1a092e4713add
- "instructions to use skia and glfw together. (download, installation, first
program). as of Sept 2023, Windows is broken but this is still sadly the best
starting resource for skia on Windows too."
-}

kWidth, kHeight :: Int
kWidth = 960
kHeight = 640

initSkia :: Int -> Int -> IO (GRDirectContext, SKSurface)
initSkia w h = do
    interface <-
        GRGlInterface.createNativeInterface >>= \case
            Nothing -> error "cannot make native GL interface"
            Just interface -> pure interface
    context <- GRDirectContext.createGl interface Nothing
    let fbinfo =
            Gr_gl_framebufferinfo
                { fFBOID = 0
                , fFormat = CUInt GL.GL_RGBA8
                , fProtected = 0
                }
    target <- GRBackendRenderTarget.createGl w h 0 0 fbinfo
    isvalid <- GRBackendRenderTarget.isValid target
    unless isvalid $ error "GL backend render target is not valid"

    Just surface <-
        SKSurface.createByWrappingBackendRenderTarget
            context
            target
            GRSurfaceOrigin'BottomLeft
            SKColorType'RGBA'8888
            Nothing
            Nothing
    pure (context, surface)

main :: IO ()
main = do
    GLFW.setErrorCallback $ Just \error description -> do
        hPutStrLn stderr $ "[GLFW ERROR " <> show error <> "] " <> description

    initOk <- GLFW.init
    unless initOk exitFailure
    putStrLn "GLFW initialized"

    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 2
    GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
    GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
    GLFW.windowHint $ GLFW.WindowHint'StencilBits (Just 0)
    GLFW.windowHint $ GLFW.WindowHint'DepthBits (Just 0)

    window <-
        GLFW.createWindow kWidth kHeight "Simple example" Nothing Nothing >>= \case
            Nothing -> GLFW.terminate *> exitFailure
            Just window -> pure window

    GLFW.makeContextCurrent (Just window)
    (context, surface) <- initSkia kWidth kHeight

    GLFW.swapInterval 0 -- This makes the canvas more responsive.

    SKSurface.withCanvas surface \canvas -> do
        fix \continue -> do
            shouldClose <- GLFW.windowShouldClose window
            unless shouldClose do
                GLFW.waitEvents

                (x, y) <- GLFW.getCursorPos window

                paint <- SKPaint.create
                SKPaint.setColor4f paint (RGBA 1 1 1 1) Nothing
                SKCanvas.drawPaint canvas paint
                SKPaint.setColor4f paint (RGBA 0 0 1 1) Nothing
                SKCanvas.drawRect canvas Rect{left = realToFrac x, top = realToFrac y, right = 300, bottom = 500} paint

                GRDirectContext.flush context
                GLFW.swapBuffers window

                continue

    GLFW.terminate
    putStrLn "GLFW terminated"