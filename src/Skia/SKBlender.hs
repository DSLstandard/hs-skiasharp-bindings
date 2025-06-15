module Skia.SKBlender where

import Skia.Internal.Prelude

-- * Creating 'SKBlender'

-- | Creates a blender that implements the specified BlendMode.
createMode :: (MonadIO m) => SKBlendMode -> m SKBlender
createMode mode = liftIO do
    blender' <- sk_blender_new_mode (marshalSKEnum mode)
    toObjectFin sk_blender_unref blender'

{- | Creates a blender that implements the following:

@
   k1 * src * dst + k2 * src + k3 * dst + k4
@
-}
createArithmetic ::
    (MonadIO m) =>
    -- | (k1, k2, k3, k4)
    (Float, Float, Float, Float) ->
    -- | \"enforcePMColor\"; If true, the RGB channels will be clamped to the calculated alpha.
    Bool ->
    m SKBlender
createArithmetic (k1, k2, k3, k4) enforcePMColor = liftIO do
    blender' <- sk_blender_new_arithmetic (coerce k1) (coerce k2) (coerce k3) (coerce k4) (fromBool enforcePMColor)
    toObjectFin sk_blender_unref blender'
