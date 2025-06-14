module SharedUtils.MakeDemoApp (
    runDemoCanvasWindowApp,
)
where

import Control.Monad
import Foreign.C
import Graphics.GL qualified as GL
import Graphics.UI.GLFW qualified as GLFW
import Linear
import Skia.Bindings (Gr_gl_framebufferinfo (..))
import Skia.GRBackendRenderTarget qualified as GRBackendRenderTarget
import Skia.GRDirectContext qualified as GRDirectContext
import Skia.GRGlInterface qualified as GRGlInterface
import Skia.SKSurface qualified as SKSurface
import Skia.Types
import System.Exit
import System.IO

-- | See 'runDemoCanvasWindowApp' for more description.
type DemoCanvasWindowApp =
    -- | App window
    GLFW.Window ->
    -- | App canvas
    SKCanvas ->
    -- | Callback to flush draw commands. This function should be called before a window update.
    IO () ->
    IO ()

{- | Creates a GLFW window with an associated canvas and an 'IO' action to flush
to underlying drawing commands, which should be called before a window update.

The input function takes these objects and shall create a GLFW loop (i.e., check
GLFW.windowShouldClose, GLFW.waitEvents, GLFW.swapBuffers, etc). When the
function ends, the window is destroyed automatically.
-}
runDemoCanvasWindowApp ::
    -- | Title of the window
    String ->
    -- | Window size
    V2 Int ->
    DemoCanvasWindowApp ->
    IO ()
runDemoCanvasWindowApp winTitle (V2 kWidth kHeight) loop = do
    -- NOTE: This is almost a direct copy of demos/DemoGLFWOpenGL.hs
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
        GLFW.createWindow kWidth kHeight winTitle Nothing Nothing >>= \case
            Nothing -> GLFW.terminate *> exitFailure
            Just window -> pure window

    GLFW.makeContextCurrent (Just window)
    (context, surface) <- initSkia kWidth kHeight

    GLFW.swapInterval 0

    SKSurface.withCanvas surface \canvas -> do
        let flush = GRDirectContext.flush context
        loop window canvas flush

    GLFW.terminate
    putStrLn "GLFW terminated"
  where
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
