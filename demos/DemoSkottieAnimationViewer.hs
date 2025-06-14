module Main where

import Control.Monad
import Control.Monad.Fix
import Data.Fixed (mod')
import Data.Text qualified as T
import Data.Time.Clock.POSIX qualified as Time
import Graphics.UI.GLFW qualified as GLFW
import Linear
import Options.Applicative qualified as Opts
import SharedUtils.MakeDemoApp
import Skia.SKCanvas qualified as SKCanvas
import Skia.SkottieAnimation qualified as SkottieAnimation
import Skia.SkottieAnimationBuilder qualified as SkottieAnimationBuilder
import Skia.Types
import System.Exit

parseArgs :: IO FilePath
parseArgs = do
    Opts.execParser $
        Opts.info
            ( Opts.helper
                <*> Opts.strOption
                    ( Opts.short 'f'
                        <> Opts.long "lottie-file"
                        <> Opts.metavar "FILE"
                        <> Opts.help "Specify the Lottie animation to display"
                        <> Opts.value "./assets/lottie-animations/lottie-logo.json"
                        <> Opts.showDefault
                    )
            )
            ( Opts.fullDesc
                <> Opts.progDesc "Demo Skottie animation viewer"
            )

main :: IO ()
main = do
    filepath <- parseArgs

    builder <- SkottieAnimationBuilder.create SkottieAnimationBuilder.defaultCreateFlags
    anim <-
        SkottieAnimationBuilder.buildFromFile builder filepath >>= \case
            Nothing -> do
                putStrLn $ "[!] Error: Cannot build SkottieAnimation from file " <> filepath
                putStrLn $ "[!] Quitting..."
                exitFailure
            Just anim -> do
                pure anim
    SkottieAnimationBuilder.destroy builder

    -- Print stats
    putStrLn "Info about the loaded animation:"

    duration <- SkottieAnimation.getDuration anim
    putStrLn $ "- Duration (seconds): " <> show duration

    V2 width height <- SkottieAnimation.getSize anim
    putStrLn $ "- Dimensions: " <> show width <> "x" <> show height

    fps <- SkottieAnimation.getFPS anim
    putStrLn $ "- Frames per second: " <> show fps

    inPoint <- SkottieAnimation.getInPoint anim
    putStrLn $ "- Animation in point (frame index units): " <> show inPoint

    outPoint <- SkottieAnimation.getInPoint anim
    putStrLn $ "- Animation out point (frame index units): " <> show outPoint

    version <- SkottieAnimation.getVersion anim
    putStrLn $ "- Version: " <> T.unpack version

    runDemoCanvasWindowApp
        ("Demo Skottie animation viewer: " <> filepath)
        (V2 (ceiling width) (ceiling height))
        \window canvas flushCanvas -> do
            startTime <- Time.getPOSIXTime

            fix \loop -> do
                shouldClose <- GLFW.windowShouldClose window
                unless shouldClose do
                    GLFW.pollEvents

                    SKCanvas.clearRGBA canvas $ RGBA 1.0 1.0 1.0 1.0

                    nowTime <- Time.getPOSIXTime
                    let t = realToFrac (nowTime - startTime) `mod'` duration
                    SkottieAnimation.seekFrameTime anim (realToFrac t) Nothing
                    SkottieAnimation.render
                        anim
                        canvas
                        Nothing
                        SkottieAnimation.defaultRenderFlags

                    flushCanvas
                    GLFW.swapBuffers window

                    loop
