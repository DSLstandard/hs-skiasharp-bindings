module Skia.SKShader where

import Linear
import Skia.Internal.Prelude

createEmpty :: (MonadIO m) => m SKShader
createEmpty = liftIO do
    shader' <- sk_shader_new_empty
    toObjectFin sk_shader_unref shader'

createColor :: (MonadIO m) => SKColor -> m SKShader
createColor color = evalContIO do
    shader' <- liftIO $ sk_shader_new_color (coerce color)
    toObjectFin sk_shader_unref shader'

createColor4f :: (MonadIO m) => RGBA Float -> SKColorSpace -> m SKShader
createColor4f color colorspace = evalContIO do
    color' <- useStorable $ toSKColor4f color
    colorspace' <- useObj colorspace
    shader' <- liftIO $ sk_shader_new_color4f color' colorspace'
    toObjectFin sk_shader_unref shader'

createBlend ::
    (MonadIO m) =>
    SKBlender ->
    -- | Blend destination shader
    SKShader ->
    -- | Blend source shader
    SKShader ->
    m SKShader
createBlend blender blendDst blendSrc = evalContIO do
    blender' <- useObj blender
    blendDst' <- useObj blendDst
    blendSrc' <- useObj blendSrc

    shader' <- liftIO $ sk_shader_new_blender blender' blendDst' blendSrc'
    toObjectFin sk_shader_unref shader'

createBlendByMode ::
    (MonadIO m) =>
    SKBlendMode ->
    -- | Blend destination shader
    SKShader ->
    -- | Blend source shader
    SKShader ->
    m SKShader
createBlendByMode blendMode blendDst blendSrc = evalContIO do
    blendDst' <- useObj blendDst
    blendSrc' <- useObj blendSrc

    shader' <- liftIO $ sk_shader_new_blend (marshalSKEnum blendMode) blendDst' blendSrc'
    toObjectFin sk_shader_unref shader'

data Gradient
    = Gradient'Linear
        { start :: V2 Float
        , end :: V2 Float
        }
    | Gradient'Radial
        { center :: V2 Float
        , radius :: Float
        }
    | Gradient'Sweep
        { center :: V2 Float
        , startAngle :: Degrees
        , endAngle :: Degrees
        }
    | Gradient'TwoPointConical
        { start :: V2 Float
        , startRadius :: Float
        , end :: V2 Float
        , endRadius :: Float
        }
    deriving (Show, Eq, Ord)

createGradientRaw ::
    (MonadIO m) =>
    Gradient ->
    -- | Color array
    InColor (Ptr Sk_color4f) (Ptr SKColor) ->
    -- | Color position array
    Ptr Float ->
    -- | Color count
    Int ->
    SKShaderTileMode ->
    -- | Local matrix
    M33 Float ->
    m SKShader
createGradientRaw gradient inColorArray colorPosArray colorCount tileMode localMatrix = evalContIO do
    -- NOTE: This function is implemented so all 4 gradient types can be
    -- implemented in < 30 lines, and not >= 500 lines.

    -- Pass gradient type specific arguments
    (newGradientColorHex, newGradientColor4f) <- case gradient of
        Gradient'Linear{start, end} -> do
            points' <- ContT $ withArray [toSKPoint start, toSKPoint end]
            pure
                ( sk_shader_new_linear_gradient points'
                , sk_shader_new_linear_gradient_color4f points'
                )
        Gradient'Radial{center, radius} -> do
            center' <- useStorable (toSKPoint center)
            pure
                ( sk_shader_new_radial_gradient center' (coerce radius)
                , sk_shader_new_radial_gradient_color4f center' (coerce radius)
                )
        Gradient'Sweep{center, startAngle, endAngle} -> do
            center' <- useStorable (toSKPoint center)
            -- For some reason the arguments startAngle & endAngle are placed at the back...
            pure
                ( \colors colorPos colorCount tileMode localMatrix ->
                    sk_shader_new_sweep_gradient center' colors colorPos colorCount tileMode (coerce startAngle) (coerce endAngle) localMatrix
                , \colors colorspace colorPos colorCount tileMode localMatrix ->
                    sk_shader_new_sweep_gradient_color4f center' colors colorspace colorPos colorCount tileMode (coerce startAngle) (coerce endAngle) localMatrix
                )
        Gradient'TwoPointConical{start, startRadius, end, endRadius} -> do
            start' <- useStorable (toSKPoint start)
            end' <- useStorable (toSKPoint end)
            pure
                ( sk_shader_new_two_point_conical_gradient start' (coerce startRadius) end' (coerce endRadius)
                , sk_shader_new_two_point_conical_gradient_color4f start' (coerce startRadius) end' (coerce endRadius)
                )

    -- Pass color specific arguments
    newShader <- case inColorArray of
        InColor'Hex colorArray' -> do
            pure $ newGradientColorHex (coercePtr colorArray')
        InColor colorArray' colorspace -> do
            colorspace' <- useObj colorspace
            pure $ newGradientColor4f colorArray' colorspace'

    -- Pass other common arguments
    localMatrix' <- useStorable $ toSKMatrix localMatrix
    shader' <- liftIO $ newShader (castPtr colorPosArray) (fromIntegral colorCount) (marshalSKEnum tileMode) localMatrix'
    toObjectFin sk_shader_unref shader'

createGradientHexByList ::
    (MonadIO m) =>
    Gradient ->
    -- | List of (color position, color hex)
    [(Float, SKColor)] ->
    SKShaderTileMode ->
    -- | Local matrix
    M33 Float ->
    m SKShader
createGradientHexByList gradient entries tileMode localMatrix = evalContIO do
    let (posArray, hexArray) = unzip entries
    (posArray', numEntries) <- ContT $ withArrayLen' posArray
    hexArray' <- ContT $ withArray hexArray
    createGradientRaw gradient (InColor'Hex hexArray') posArray' numEntries tileMode localMatrix

createGradientByList ::
    (MonadIO m) =>
    Gradient ->
    -- | List of (color position, color hex)
    [(Float, RGBA Float)] ->
    SKColorSpace ->
    SKShaderTileMode ->
    -- | Local matrix
    M33 Float ->
    m SKShader
createGradientByList gradient entries colorspace tileMode localMatrix = evalContIO do
    let (posArray, colorArray) = unzip entries
    (posArray', numEntries) <- ContT $ withArrayLen' posArray
    colorArray' <- ContT $ withArray $ fmap toSKColor4f colorArray
    createGradientRaw gradient (InColor colorArray' colorspace) posArray' numEntries tileMode localMatrix

createPerlinNoiseFractualNoise ::
    (MonadIO m) =>
    -- | Base frequency X, Y
    V2 Float ->
    -- | Num octaves
    Int ->
    -- | Seed
    Float ->
    -- | Tile size
    V2 Int ->
    m SKShader
createPerlinNoiseFractualNoise (V2 fx fy) numOctaves seed tileSize = evalContIO do
    tileSize' <- useStorable $ toSKISize tileSize
    shader' <- liftIO $ sk_shader_new_perlin_noise_fractal_noise (coerce fx) (coerce fy) (fromIntegral numOctaves) (coerce seed) tileSize'
    toObjectFin sk_shader_unref shader'

createPerlinNoiseTurbulence ::
    (MonadIO m) =>
    -- | Base frequency X, Y
    V2 Float ->
    -- | Num octaves
    Int ->
    -- | Seed
    Float ->
    -- | Tile size
    V2 Int ->
    m SKShader
createPerlinNoiseTurbulence (V2 fx fy) numOctaves seed tileSize = evalContIO do
    tileSize' <- useStorable $ toSKISize tileSize
    shader' <- liftIO $ sk_shader_new_perlin_noise_turbulence (coerce fx) (coerce fy) (fromIntegral numOctaves) (coerce seed) tileSize'
    toObjectFin sk_shader_unref shader'

withLocalMatrix :: (MonadIO m) => SKShader -> M33 Float -> m SKShader
withLocalMatrix shader localMatrix = evalContIO do
    shader' <- useObj shader
    localMatrix' <- useStorable $ toSKMatrix localMatrix

    newShader' <- liftIO $ sk_shader_with_local_matrix shader' localMatrix'
    toObjectFin sk_shader_unref newShader'

withColorFilter :: (MonadIO m) => SKShader -> SKColorFilter -> m SKShader
withColorFilter shader colorFilter = evalContIO do
    shader' <- useObj shader
    colorFilter' <- useObj colorFilter

    newShader' <- liftIO $ sk_shader_with_color_filter shader' colorFilter'
    toObjectFin sk_shader_unref newShader'
