module Main where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Acquire
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

    runResourceT do
        (builderKey, builder) <- allocateAcquire $ SkottieAnimationBuilder.create SkottieAnimationBuilder.defaultCreateFlags
        (_animKey, anim) <-
            SkottieAnimationBuilder.buildFromFile builder filepath >>= \case
                Nothing -> do
                    liftIO $ putStrLn $ "[!] Error: Cannot build SkottieAnimation from file " <> filepath
                    liftIO $ putStrLn $ "[!] Quitting..."
                    liftIO $ exitFailure
                Just anim -> do
                    pure anim
        release builderKey

        -- Print stats
        duration <- SkottieAnimation.getDuration anim
        V2 width height <- SkottieAnimation.getSize anim
        fps <- SkottieAnimation.getFPS anim
        inPoint <- SkottieAnimation.getInPoint anim
        outPoint <- SkottieAnimation.getInPoint anim
        version <- SkottieAnimation.getVersion anim

        liftIO $ putStrLn "Info about the loaded animation:"
        liftIO $ putStrLn $ "- Duration (seconds): " <> show duration
        liftIO $ putStrLn $ "- Dimensions: " <> show width <> "x" <> show height
        liftIO $ putStrLn $ "- Frames per second: " <> show fps
        liftIO $ putStrLn $ "- Animation in point (frame index units): " <> show inPoint
        liftIO $ putStrLn $ "- Animation out point (frame index units): " <> show outPoint
        liftIO $ putStrLn $ "- Version: " <> T.unpack version

        let winTitle = "Demo Skottie animation viewer: " <> filepath
        let winSize = V2 (ceiling width) (ceiling height)
        liftIO $ runDemoCanvasWindowApp winTitle winSize \window canvas flushCanvas -> do
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
