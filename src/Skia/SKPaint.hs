module Skia.SKPaint where

import Linear
import Skia.Internal.Prelude
import Skia.SKPath qualified as SKPath

delete :: (MonadIO m) => SKPaint -> m ()
delete paint = evalContIO do
    paint' <- useObj paint
    liftIO $ sk_paint_delete paint'

create :: (MonadIO m) => m SKPaint
create = evalContIO do
    paint' <- liftIO $ sk_paint_new
    toObject paint'

clone :: (MonadIO m) => SKPaint -> m SKPaint
clone paint = evalContIO do
    paint' <- useObj paint
    newPaint' <- liftIO $ sk_paint_clone paint'
    toObject newPaint'

reset :: (MonadIO m) => SKPaint -> m ()
reset paint = evalContIO do
    paint' <- useObj paint
    liftIO $ sk_paint_reset paint'

isAntialias :: (MonadIO m) => SKPaint -> m Bool
isAntialias paint = evalContIO do
    paint' <- useObj paint
    r <- liftIO $ sk_paint_is_antialias paint'
    pure $ toBool r

setAntialias :: (MonadIO m) => SKPaint -> Bool -> m ()
setAntialias paint antialias = evalContIO do
    paint' <- useObj paint
    liftIO $ sk_paint_set_antialias paint' (fromBool antialias)

isDither :: (MonadIO m) => SKPaint -> m Bool
isDither paint = evalContIO do
    paint' <- useObj paint
    r <- liftIO $ sk_paint_is_dither paint'
    pure $ toBool r

setDither :: (MonadIO m) => SKPaint -> Bool -> m ()
setDither paint dither = evalContIO do
    paint' <- useObj paint
    liftIO $ sk_paint_set_dither paint' (fromBool dither)

getStyle :: (MonadIO m) => SKPaint -> m SKPaintStyle
getStyle paint = evalContIO do
    paint' <- useObj paint
    r <- liftIO $ sk_paint_get_style paint'
    unmarshalSKEnumOrDie r

setStyle :: (MonadIO m) => SKPaint -> SKPaintStyle -> m ()
setStyle paint style = evalContIO do
    paint' <- useObj paint
    liftIO $ sk_paint_set_style paint' (marshalSKEnum style)

getColor :: (MonadIO m) => SKPaint -> m SKColor
getColor paint = evalContIO do
    paint' <- useObj paint
    liftIO $ fmap coerce $ sk_paint_get_color paint'

setColor :: (MonadIO m) => SKPaint -> SKColor -> m ()
setColor paint color = evalContIO do
    paint' <- useObj paint
    liftIO $ sk_paint_set_color paint' (coerce color)

getColor4f :: (MonadIO m) => SKPaint -> m (RGBA Float)
getColor4f paint = evalContIO do
    paint' <- useObj paint
    color' <- useAlloca
    liftIO $ sk_paint_get_color4f paint' color'
    liftIO $ fromSKColor4f <$> peek color'

setColor4f :: (MonadIO m) => SKPaint -> RGBA Float -> Maybe SKColorSpace -> m ()
setColor4f paint rgba colorspace = evalContIO do
    paint' <- useObj paint
    color' <- useStorable (toSKColor4f rgba)
    cs' <- useNullIfNothing useObj colorspace
    liftIO $ sk_paint_set_color4f paint' color' cs'

getStrokeWidth :: (MonadIO m) => SKPaint -> m Float
getStrokeWidth paint = evalContIO do
    paint' <- useObj paint
    r <- liftIO $ sk_paint_get_stroke_width paint'
    pure $ coerce r

setStrokeWidth :: (MonadIO m) => SKPaint -> Float -> m ()
setStrokeWidth paint width = evalContIO do
    paint' <- useObj paint
    liftIO $ sk_paint_set_stroke_width paint' (coerce width)

getStrokeMiter :: (MonadIO m) => SKPaint -> m Float
getStrokeMiter paint = evalContIO do
    paint' <- useObj paint
    r <- liftIO $ sk_paint_get_stroke_miter paint'
    pure $ coerce r

setStrokeMiter :: (MonadIO m) => SKPaint -> Float -> m ()
setStrokeMiter paint miter = evalContIO do
    paint' <- useObj paint
    liftIO $ sk_paint_set_stroke_miter paint' (coerce miter)

getStrokeCap :: (MonadIO m) => SKPaint -> m SKStrokeCap
getStrokeCap paint = evalContIO do
    paint' <- useObj paint
    r <- liftIO $ sk_paint_get_stroke_cap paint'
    unmarshalSKEnumOrDie r

setStrokeCap :: (MonadIO m) => SKPaint -> SKStrokeCap -> m ()
setStrokeCap paint cap = evalContIO do
    paint' <- useObj paint
    liftIO $ sk_paint_set_stroke_cap paint' (marshalSKEnum cap)

getStrokeJoin :: (MonadIO m) => SKPaint -> m SKStrokeJoin
getStrokeJoin paint = evalContIO do
    paint' <- useObj paint
    r <- liftIO $ sk_paint_get_stroke_join paint'
    unmarshalSKEnumOrDie r

setStrokeJoin :: (MonadIO m) => SKPaint -> SKStrokeJoin -> m ()
setStrokeJoin paint join = evalContIO do
    paint' <- useObj paint
    liftIO $ sk_paint_set_stroke_join paint' (marshalSKEnum join)

getShader :: (MonadIO m) => SKPaint -> m SKShader
getShader paint = evalContIO do
    paint' <- useObj paint
    shader' <- liftIO $ sk_paint_get_shader paint'
    toObject shader'

setShader :: (MonadIO m) => SKPaint -> SKShader -> m ()
setShader paint shader = evalContIO do
    paint' <- useObj paint
    shader' <- useObj shader
    liftIO $ sk_paint_set_shader paint' shader'

getMaskFilter :: (MonadIO m) => SKPaint -> m (Maybe SKMaskFilter)
getMaskFilter paint = evalContIO do
    paint' <- useObj paint
    maskFilter' <- liftIO $ sk_paint_get_maskfilter paint'
    toObjectMaybe maskFilter'

setMaskFilter :: (MonadIO m) => SKPaint -> Maybe SKMaskFilter -> m ()
setMaskFilter paint maskFilter = evalContIO do
    paint' <- useObj paint
    maskFilter' <- useNullIfNothing useObj maskFilter
    liftIO $ sk_paint_set_maskfilter paint' maskFilter'

getColorFilter :: (MonadIO m) => SKPaint -> m (Maybe SKColorFilter)
getColorFilter paint = evalContIO do
    paint' <- useObj paint
    colorFilter' <- liftIO $ sk_paint_get_colorfilter paint'
    toObjectMaybe colorFilter'

setColorFilter :: (MonadIO m) => SKPaint -> Maybe SKColorFilter -> m ()
setColorFilter paint colorFilter = evalContIO do
    paint' <- useObj paint
    colorFilter' <- useNullIfNothing useObj colorFilter
    liftIO $ sk_paint_set_colorfilter paint' colorFilter'

getImageFilter :: (MonadIO m) => SKPaint -> m (Maybe SKImageFilter)
getImageFilter paint = evalContIO do
    paint' <- useObj paint
    imageFilter' <- liftIO $ sk_paint_get_imagefilter paint'
    toObjectMaybe imageFilter'

setImageFilter :: (MonadIO m) => SKPaint -> Maybe SKImageFilter -> m ()
setImageFilter paint imageFilter = evalContIO do
    paint' <- useObj paint
    imageFilter' <- useNullIfNothing useObj imageFilter
    liftIO $ sk_paint_set_imagefilter paint' imageFilter'

getBlendMode :: (MonadIO m) => SKPaint -> m SKBlendMode
getBlendMode paint = evalContIO do
    paint' <- useObj paint
    r <- liftIO $ sk_paint_get_blendmode paint'
    unmarshalSKEnumOrDie r

setBlendMode :: (MonadIO m) => SKPaint -> SKBlendMode -> m ()
setBlendMode paint blendMode = evalContIO do
    paint' <- useObj paint
    liftIO $ sk_paint_set_blendmode paint' (marshalSKEnum blendMode)

getBlender :: (MonadIO m) => SKPaint -> m (Maybe SKBlender)
getBlender paint = evalContIO do
    paint' <- useObj paint
    blender' <- liftIO $ sk_paint_get_blender paint'
    toObjectMaybe blender'

setBlender :: (MonadIO m) => SKPaint -> Maybe SKBlender -> m ()
setBlender paint blender = evalContIO do
    paint' <- useObj paint
    blender' <- useNullIfNothing useObj blender
    liftIO $ sk_paint_set_blender paint' blender'

getPathEffect :: (MonadIO m) => SKPaint -> m (Maybe SKPathEffect)
getPathEffect paint = evalContIO do
    paint' <- useObj paint
    pathEffect' <- liftIO $ sk_paint_get_path_effect paint'
    toObjectMaybe pathEffect'

setPathEffect :: (MonadIO m) => SKPaint -> Maybe SKPathEffect -> m ()
setPathEffect paint pathEffect = evalContIO do
    paint' <- useObj paint
    pathEffect' <- useNullIfNothing useObj pathEffect
    liftIO $ sk_paint_set_path_effect paint' pathEffect'

{- | Applies any and all effects to a source path, returning the result in the
destination.

Returns true if the path should be filled, or false if it should be drawn with a
hairline.
-}
getFillPathToDest ::
    (MonadIO m) =>
    SKPaint ->
    -- | Source path
    SKPath ->
    -- | Destination path
    SKPath ->
    -- | The destination path may be culled to this rectangle.
    Maybe (Rect Float) ->
    -- | Transformation matrix
    M33 Float ->
    m Bool
getFillPathToDest paint src dst cullRect transform = evalContIO do
    paint' <- useObj paint
    src' <- useObj src
    dst' <- useObj dst
    cullRect' <- useNullIfNothing useStorable $ toSKRect <$> cullRect
    transform' <- useStorable $ toSKMatrix transform

    r <- liftIO $ sk_paint_get_fill_path paint' src' dst' cullRect' transform'

    pure $ toBool r

{- | Creates a new path from the result of applying any and all effects to a
source path.

Returns the resulting fill path, or 'Nothing' if the source path should be
drawn with a hairline.

Also see 'getFillPathToDest'.
-}
getFillPath ::
    (MonadIO m) =>
    SKPaint ->
    -- | Source path
    SKPath ->
    -- | The destination path may be culled to this rectangle.
    Maybe (Rect Float) ->
    -- | Transformation matrix
    M33 Float ->
    m (Maybe SKPath)
getFillPath paint src cullRect transform = do
    dst <- SKPath.create
    shouldFill <- getFillPathToDest paint src dst cullRect transform
    if shouldFill
        then do
            pure (Just dst)
        else do
            -- Should be drawn with a hairline... return Nothing
            disposeObject dst
            pure Nothing
