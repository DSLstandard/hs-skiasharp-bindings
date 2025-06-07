module Skia.SKColorSpace where

import Control.Monad.IO.Class
import Skia.Internal.Prelude

createSRGB :: (MonadIO m) => m SKColorSpace
createSRGB = liftIO do
    space' <- sk_colorspace_new_srgb
    toObjectFin sk_colorspace_unref space'

createSRGBLinear :: (MonadIO m) => m SKColorSpace
createSRGBLinear = liftIO do
    space' <- sk_colorspace_new_srgb_linear
    toObjectFin sk_colorspace_unref space'

createRGB :: (MonadIO m) => Sk_colorspace_transfer_fn -> Sk_colorspace_xyz -> m SKColorSpace
createRGB transferFn toXYZD50 = evalContIO do
    transferFn' <- useStorable transferFn
    toXYZD50' <- useStorable toXYZD50
    liftIO $
        toObjectFin sk_colorspace_unref
            =<< sk_colorspace_new_rgb transferFn' toXYZD50'

createICC :: (MonadIO m) => SKColorSpaceICCProfile -> m SKColorSpace
createICC iccprof = evalContIO do
    iccprof' <- useObj iccprof
    liftIO $ toObjectFin sk_colorspace_unref =<< sk_colorspace_new_icc iccprof'

isGammaCloseToSRGB :: (MonadIO m) => SKColorSpace -> m Bool
isGammaCloseToSRGB space = evalContIO do
    space' <- useObj space
    liftIO $ fmap toBool $ sk_colorspace_gamma_close_to_srgb space'

isGammaLinear :: (MonadIO m) => SKColorSpace -> m Bool
isGammaLinear space = evalContIO do
    space' <- useObj space
    liftIO $ fmap toBool $ sk_colorspace_gamma_is_linear space'

isSRGB :: (MonadIO m) => SKColorSpace -> m Bool
isSRGB space = evalContIO do
    space' <- useObj space
    liftIO $ fmap toBool $ sk_colorspace_is_srgb space'

getXYZD50 :: (MonadIO m) => SKColorSpace -> m (Maybe Sk_colorspace_xyz)
getXYZD50 space = evalContIO do
    space' <- useObj space
    xyzd50' <- useAlloca

    success <-
        liftIO $
            fmap toBool $
                sk_colorspace_to_xyzd50
                    space'
                    xyzd50'

    if success
        then do
            Just <$> peekWith id xyzd50'
        else do
            pure Nothing

-- getNumericalTransferFn :: (MonadIO m) => SKColorSpace -> m (Maybe Sk_colorspace_xyz)
-- getNumericalTransferFn space = evalContIO do
--     space' <- useObj space
--     transferFn' <- useAlloca
--     success <- liftIO $ toBool <$> sk_colorspace_is_numerical_transfer_fn space' transferFn'
--     if success
--         then do
--             transferFn <- liftIO $ peek transferFn'
--             pure $ Just transferFn
--         else do
--             pure Nothing
--
-- data NamedTransferFn
--     = NamedTransferFn'SRGB
--     | NamedTransferFn'2Dot2
--     | NamedTransferFn'Linear
--     | NamedTransferFn'Rec2020
--     | NamedTransferFn'PQ
--     | NamedTransferFn'HLG
--     deriving (Show, Eq, Ord, Enum, Bounded)
