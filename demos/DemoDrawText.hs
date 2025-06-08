module Main where

import Control.Monad
import Control.Monad.Fix
import Data.Text qualified as T
import Data.Time.Clock qualified as Time
import Graphics.UI.GLFW qualified as GLFW
import Linear
import SharedUtils.MakeDemoApp
import Skia.SKCanvas qualified as SKCanvas
import Skia.SKFont qualified as SKFont
import Skia.SKFontManager qualified as SKFontManager
import Skia.SKPaint qualified as SKPaint
import Skia.Types

main :: IO ()
main = runDemoCanvasWindowApp
    "Draw text demo"
    (V2 1000 300)
    \window canvas flushCanvas -> do
        fontmgr <- SKFontManager.createDefault
        Just typeface <- SKFontManager.matchFamilyStyle fontmgr Nothing fontStyle'Normal
        font <- SKFont.createWithValues typeface 24.0 1.0 0.0

        paint <- SKPaint.create
        SKPaint.setColorRGBA paint (RGBA 0.0 0.0 0.0 1.0) Nothing

        fix \loop -> do
            shouldClose <- GLFW.windowShouldClose window
            unless shouldClose do
                GLFW.pollEvents

                currTime <- Time.getCurrentTime

                SKCanvas.clearRGBA canvas $ RGBA 1.0 1.0 1.0 1.0
                SKCanvas.drawTextSimpleByText
                    canvas
                    ("Hello world!! Right now it is " <> T.pack (show currTime))
                    (V2 100 100)
                    font
                    paint

                flushCanvas
                GLFW.swapBuffers window

                loop