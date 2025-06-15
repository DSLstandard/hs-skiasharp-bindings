module Main where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Acquire
import Foreign hiding (void)
import Data.Text qualified as T
import Foreign.C
import Graphics.GL qualified as GL
import qualified SDL
import qualified SDL.Raw
import Linear
import Skia.Bindings
import Skia.GRBackendRenderTarget qualified as GRBackendRenderTarget
import Skia.GRDirectContext qualified as GRDirectContext
import Skia.GRGlInterface qualified as GRGlInterface
import Skia.SKCanvas qualified as SKCanvas
import Skia.SKFont qualified as SKFont
import Skia.SKPaint qualified as SKPaint
import Skia.SKSurface qualified as SKSurface
import Skia.Types


{-
Demo of setting up Skia on SDL2 with an OpenGL backend.

This demo's implementation is translated and adapted from Kevin Yin
(<https://github.com/ad8e>)'s Github GIST here:
https://gist.github.com/ad8e/dd150b775ae6aa4d5cf1a092e4713add
- "instructions to use skia and glfw together. (download, installation, first
program). as of Sept 2023, Windows is broken but this is still sadly the best
starting resource for skia on Windows too."
-}

kWidth, kHeight :: Int
kWidth = 960
kHeight = 640

initSkia :: (MonadResource m) => Int -> Int -> m (GRDirectContext, SKSurface)
initSkia w h = do
    (_, interface) <- allocateAcquire $ GRGlInterface.createNativeInterface
    (_, context) <- allocateAcquire $ GRDirectContext.createGl interface Nothing

    let fbinfo =
            Gr_gl_framebufferinfo
                { fFBOID = 0
                , fFormat = CUInt GL.GL_RGBA8
                , fProtected = 0
                }
    (_, target) <- allocateAcquire $ GRBackendRenderTarget.createGl w h 0 0 fbinfo
    isvalid <- GRBackendRenderTarget.isValid target
    unless isvalid $ error "GL backend render target is not valid"

    (_, surface) <-
        allocateAcquire $
            SKSurface.wrapBackendRenderTarget
                context
                target
                GRSurfaceOrigin'BottomLeft
                SKColorType'RGBA'8888
                Nothing
                Nothing
    pure (context, surface)

getSdlMousePos :: MonadIO m => m (V2 Int)
getSdlMousePos = liftIO do
    alloca \x' -> alloca \y' -> do
        _ <- SDL.Raw.getMouseState x' y'
        x <- peek x'
        y <- peek y'
        pure $ fmap fromIntegral $ V2 x y

main :: IO ()
main = do
    -- Setup SDL2 things...
    SDL.initializeAll
    let
        winGraphicsConf =
            SDL.OpenGLContext
                SDL.defaultOpenGL
                    { SDL.glProfile = SDL.Compatibility SDL.Normal 3 2
                    , SDL.glStencilPrecision = 0
                    , SDL.glMultisampleSamples = 0
                    }
        winConf =
            SDL.defaultWindow
                { SDL.windowGraphicsContext = winGraphicsConf
                , SDL.windowResizable = True
                , SDL.windowInitialSize = V2 (fromIntegral kWidth) (fromIntegral kHeight)
                }
    window <- SDL.createWindow "Demo SDL2 OpenGL Skia" winConf

    glctx <- SDL.glCreateContext window
    SDL.glMakeCurrent window glctx
    void $ SDL.Raw.glSetSwapInterval 0 -- This makes the canvas refresh faster and more responsive.

    runResourceT do
        (context, surface) <- initSkia kWidth kHeight
        (_, canvas) <- allocateAcquire $ SKSurface.getCanvas surface

        -- Setup font for drawing text
        (_, font) <- allocateAcquire $ SKFont.create
        SKFont.setSize font 48
        SKFont.setSkewX font (-0.5)

        let
            isEventQuit :: SDL.Event -> Bool
            isEventQuit event =
                case SDL.eventPayload event of
                    SDL.WindowClosedEvent _ -> True
                    _ -> False

        fix \continue -> do
            events <- SDL.pollEvents

            let shouldQuit = any isEventQuit events

            unless shouldQuit do
                V2 x y <- getSdlMousePos

                -- Nest another ResourceT to isolate SKObjects related to
                -- drawing
                runResourceT do
                    (_, paint) <- allocateAcquire SKPaint.create
                    SKPaint.setColorRGBA paint (RGBA 1 1 1 1) Nothing
                    SKCanvas.drawPaint canvas paint

                    -- Draw a blue rectangle for testing
                    let rect =
                            Rect
                                { left = realToFrac x
                                , top = realToFrac y
                                , right = 300
                                , bottom = 500
                                }
                    SKPaint.setColorRGBA paint (RGBA 0 0 1 1) Nothing
                    SKCanvas.drawRect canvas rect paint

                    -- Draw text
                    SKPaint.setColorRGBA paint (RGBA 0 0 0 1) Nothing
                    SKCanvas.drawSimpleText
                        canvas
                        (T.pack $ "Mouse = " <> show (x, y))
                        (fmap realToFrac $ V2 x y)
                        font
                        paint


                -- The previous draw operations/commands must be flushed before
                -- SDL.glSwapWindow, otherwise you see nothing.
                GRDirectContext.flush context
                SDL.glSwapWindow window

                continue

    SDL.quit
    putStrLn "SDL2 terminated"