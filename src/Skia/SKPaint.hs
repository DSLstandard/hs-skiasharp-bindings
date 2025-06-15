module Skia.SKPaint where

import Control.Monad.Trans.Resource
import Linear
import Skia.Internal.Prelude

create :: Acquire SKPaint
create = mkSKObjectAcquire sk_paint_new sk_paint_delete

clone :: SKPaint -> Acquire SKPaint
clone paint =
    mkSKObjectAcquire
        (sk_paint_clone (ptr paint))
        sk_paint_delete

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
getColor paint = liftIO do
    fmap coerce $ sk_paint_get_color (ptr paint)

setColor :: (MonadIO m) => SKPaint -> SKColor -> m ()
setColor paint color = liftIO do
    sk_paint_set_color (ptr paint) (coerce color)

getColorRGBA :: (MonadIO m) => SKPaint -> m (RGBA Float)
getColorRGBA paint = evalContIO do
    color' <- useAlloca
    liftIO $ sk_paint_get_color4f (ptr paint) color'
    liftIO $ fromSKColor4f <$> peek color'

setColorRGBA ::
    (MonadIO m) =>
    SKPaint ->
    RGBA Float ->
    Maybe SKColorSpace ->
    m ()
setColorRGBA paint rgba colorspace = evalContIO do
    rgba' <- useStorable (toSKColor4f rgba)
    liftIO $ sk_paint_set_color4f (ptr paint) rgba' (ptrOrNull colorspace)

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

getShader ::
    (MonadResource m) =>
    SKPaint ->
    -- | returns SkShader if previously set, 'Nothing' otherwise
    m (Maybe (ReleaseKey, SKShader))
getShader paint = do
    -- NOTE: sk_paint_get_shader does ref()
    allocateSKObjectUnlessNull
        (sk_paint_get_shader (ptr paint))
        sk_shader_unref

setShader :: (MonadIO m) => SKPaint -> Maybe SKShader -> m ()
setShader paint shader = liftIO do
    sk_paint_set_shader (ptr paint) (ptrOrNull shader)

getMaskFilter :: (MonadResource m) => SKPaint -> m (Maybe (ReleaseKey, SKMaskFilter))
getMaskFilter paint =
    -- NOTE: sk_paint_get_maskfilter does ref()
    allocateSKObjectUnlessNull
        (sk_paint_get_maskfilter (ptr paint))
        sk_maskfilter_unref

setMaskFilter :: (MonadIO m) => SKPaint -> Maybe SKMaskFilter -> m ()
setMaskFilter paint maskFilter = liftIO do
    sk_paint_set_maskfilter (ptr paint) (ptrOrNull maskFilter)

getColorFilter :: (MonadResource m) => SKPaint -> m (Maybe (ReleaseKey, SKColorFilter))
getColorFilter paint =
    -- NOTE: sk_paint_get_colorfilter does ref()
    allocateSKObjectUnlessNull
        (sk_paint_get_colorfilter (ptr paint))
        sk_colorfilter_unref

setColorFilter :: (MonadIO m) => SKPaint -> Maybe SKColorFilter -> m ()
setColorFilter paint colorFilter = evalContIO do
    paint' <- useObj paint
    colorFilter' <- useNullIfNothing useObj colorFilter
    liftIO $ sk_paint_set_colorfilter paint' colorFilter'

getImageFilter :: (MonadResource m) => SKPaint -> m (Maybe (ReleaseKey, SKImageFilter))
getImageFilter paint =
    -- NOTE: sk_paint_get_colorfilter does ref()
    allocateSKObjectUnlessNull
        (sk_paint_get_imagefilter (ptr paint))
        sk_imagefilter_unref

setImageFilter :: (MonadIO m) => SKPaint -> Maybe SKImageFilter -> m ()
setImageFilter paint imageFilter = liftIO do
    liftIO $ sk_paint_set_imagefilter (ptr paint) (ptrOrNull imageFilter)

getBlendMode :: (MonadIO m) => SKPaint -> m SKBlendMode
getBlendMode paint = liftIO do
    r <- sk_paint_get_blendmode (ptr paint)
    unmarshalSKEnumOrDie r

setBlendMode :: (MonadIO m) => SKPaint -> SKBlendMode -> m ()
setBlendMode paint blendMode = liftIO do
    liftIO $ sk_paint_set_blendmode (ptr paint) (marshalSKEnum blendMode)

getBlender :: (MonadResource m) => SKPaint -> m (Maybe (ReleaseKey, SKBlender))
getBlender paint =
    -- sk_paint_get_blender uses refBlender(), which +1 to refcnt
    allocateSKObjectUnlessNull
        (sk_paint_get_blender (ptr paint))
        sk_blender_unref

setBlender :: (MonadIO m) => SKPaint -> Maybe SKBlender -> m ()
setBlender paint blender = liftIO do
    sk_paint_set_blender (ptr paint) (ptrOrNull blender)

getPathEffect :: (MonadResource m) => SKPaint -> m (Maybe (ReleaseKey, SKPathEffect))
getPathEffect paint =
    -- sk_paint_get_path_effect uses refPathEffect(), which +1 to refcnt
    allocateSKObjectUnlessNull
        (sk_paint_get_path_effect (ptr paint))
        sk_path_effect_unref

setPathEffect :: (MonadIO m) => SKPaint -> Maybe SKPathEffect -> m ()
setPathEffect paint pathEffect = liftIO do
    sk_paint_set_path_effect (ptr paint) (ptrOrNull pathEffect)

{- | Applies any and all effects to a source path, returning the result in the
destination.

Returns true if the path should be filled, or false if it should be drawn with a
hairline.
-}
getFillPath ::
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
getFillPath paint src dst cullRect transform = evalContIO do
    cullRect' <- useNullIfNothing useStorable $ toSKRect <$> cullRect
    transform' <- useStorable $ toSKMatrix transform
    r <- liftIO $ sk_paint_get_fill_path (ptr paint) (ptr src) (ptr dst) cullRect' transform'
    pure $ toBool r
