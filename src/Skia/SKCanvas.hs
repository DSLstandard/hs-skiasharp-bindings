module Skia.SKCanvas where

import Linear
import Skia.Internal.Prelude
import Skia.Types.Rect qualified as Rect

newtype StackDepth = StackDepth {unStackDepth :: Int}
    deriving (Show, Eq, Ord)
    deriving newtype (Num)

destroy :: (MonadIO m, IsSKCanvas canvas) => canvas -> m ()
destroy (toA SKCanvas -> canvas) = evalContIO do
    canvas' <- useObj canvas
    liftIO $ sk_canvas_destroy canvas'

clearHex :: (MonadIO m, IsSKCanvas canvas) => canvas -> SKColor -> m ()
clearHex (toA SKCanvas -> canvas) hex = evalContIO do
    canvas' <- useObj canvas
    liftIO $ sk_canvas_clear canvas' (coerce hex)

clear :: (MonadIO m, IsSKCanvas canvas) => canvas -> RGBA Float -> m ()
clear (toA SKCanvas -> canvas) color = evalContIO do
    canvas' <- useObj canvas
    liftIO $ sk_canvas_clear_color4f canvas' (toSKColor4f color)

discard :: (MonadIO m, IsSKCanvas canvas) => canvas -> m ()
discard (toA SKCanvas -> canvas) = evalContIO do
    canvas' <- useObj canvas
    liftIO $ sk_canvas_discard canvas'

getSaveCount :: (MonadIO m, IsSKCanvas canvas) => canvas -> m StackDepth
getSaveCount (toA SKCanvas -> canvas) = evalContIO do
    canvas' <- useObj canvas
    liftIO $ fmap fromIntegral $ sk_canvas_get_save_count canvas'

restoreToCount :: (MonadIO m, IsSKCanvas canvas) => canvas -> StackDepth -> m ()
restoreToCount (toA SKCanvas -> canvas) (StackDepth saveCount) = evalContIO do
    canvas' <- useObj canvas
    liftIO $ sk_canvas_restore_to_count canvas' (fromIntegral saveCount)

drawColorHex :: (MonadIO m, IsSKCanvas canvas) => canvas -> SKColor -> SKBlendMode -> m ()
drawColorHex (toA SKCanvas -> canvas) hex blendMode = evalContIO do
    canvas' <- useObj canvas
    liftIO $ sk_canvas_draw_color canvas' (coerce hex) (marshalSKEnum blendMode)

drawColor :: (MonadIO m, IsSKCanvas canvas) => canvas -> RGBA Float -> SKBlendMode -> m ()
drawColor (toA SKCanvas -> canvas) color blendMode = evalContIO do
    canvas' <- useObj canvas
    liftIO $ sk_canvas_draw_color4f canvas' (toSKColor4f color) (marshalSKEnum blendMode)

drawPointsRaw ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    SKPointMode ->
    -- | Number of points
    Int ->
    -- | Points array
    Ptr Sk_point ->
    SKPaint ->
    m ()
drawPointsRaw (toA SKCanvas -> canvas) pointMode numPoints pointsArray paint = evalContIO do
    canvas' <- useObj canvas
    paint' <- useObj paint
    liftIO $
        sk_canvas_draw_points
            canvas'
            (marshalSKEnum pointMode)
            (fromIntegral numPoints)
            pointsArray
            paint'

drawPoint :: (MonadIO m, IsSKCanvas canvas) => canvas -> V2 Float -> SKPaint -> m ()
drawPoint (toA SKCanvas -> canvas) (V2 x y) paint = evalContIO do
    canvas' <- useObj canvas
    paint' <- useObj paint
    liftIO $
        sk_canvas_draw_point
            canvas'
            (coerce x)
            (coerce y)
            paint'

drawLine :: (MonadIO m, IsSKCanvas canvas) => canvas -> V2 Float -> V2 Float -> SKPaint -> m ()
drawLine (toA SKCanvas -> canvas) (V2 x0 y0) (V2 x1 y1) paint = evalContIO do
    canvas' <- useObj canvas
    paint' <- useObj paint
    liftIO $
        sk_canvas_draw_line
            canvas'
            (coerce x0)
            (coerce y0)
            (coerce x1)
            (coerce y1)
            paint'

drawSimpleTextRaw ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | Text data
    Ptr Word8 ->
    -- | Byte length of text data
    Int ->
    SKTextEncoding ->
    -- | Position
    V2 Float ->
    SKFont ->
    SKPaint ->
    m ()
drawSimpleTextRaw (toA SKCanvas -> canvas) textData textDataLen textEncoding (V2 x y) font paint = evalContIO do
    canvas' <- useObj canvas
    font' <- useObj font
    paint' <- useObj paint
    liftIO $
        sk_canvas_draw_simple_text
            canvas'
            (castPtr textData)
            (fromIntegral textDataLen)
            (marshalSKEnum textEncoding)
            (coerce x)
            (coerce y)
            font'
            paint'

drawTextBlob :: (MonadIO m, IsSKCanvas canvas) => canvas -> SKTextBlob -> V2 Float -> SKPaint -> m ()
drawTextBlob (toA SKCanvas -> canvas) textBlob (V2 x y) paint = evalContIO do
    canvas' <- useObj canvas
    textBlob' <- useObj textBlob
    paint' <- useObj paint
    liftIO $
        sk_canvas_draw_text_blob
            canvas'
            textBlob'
            (coerce x)
            (coerce y)
            paint'

resetMatrix :: (MonadIO m, IsSKCanvas canvas) => canvas -> m ()
resetMatrix (toA SKCanvas -> canvas) = evalContIO do
    canvas' <- useObj canvas
    liftIO $ sk_canvas_reset_matrix canvas'

setMatrix :: (MonadIO m, IsSKCanvas canvas) => canvas -> M44 Float -> m ()
setMatrix (toA SKCanvas -> canvas) matrix = evalContIO do
    canvas' <- useObj canvas
    matrix' <- useStorable (toSKMatrix44 matrix)
    liftIO $ sk_canvas_set_matrix canvas' matrix'

getMatrix :: (MonadIO m, IsSKCanvas canvas) => canvas -> m (M44 Float)
getMatrix (toA SKCanvas -> canvas) = evalContIO do
    canvas' <- useObj canvas
    matrix' <- useAlloca
    liftIO $ sk_canvas_get_matrix canvas' matrix'
    peekWith fromSKMatrix44 matrix'

drawRoundRectByRect ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    Rect Float ->
    -- | X Y radius
    V2 Float ->
    SKPaint ->
    m ()
drawRoundRectByRect (toA SKCanvas -> canvas) rect (V2 rx ry) paint = evalContIO do
    canvas' <- useObj canvas
    rect' <- useStorable (Rect.toSKRect rect)
    paint' <- useObj paint
    liftIO $
        sk_canvas_draw_round_rect
            canvas'
            rect'
            (coerce rx)
            (coerce ry)
            paint'

clipRectWithOp ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    Rect Float ->
    SKClipOp ->
    -- | Do antialiasing?
    Bool ->
    m ()
clipRectWithOp (toA SKCanvas -> canvas) rect op doAA = evalContIO do
    canvas' <- useObj canvas
    rect' <- useStorable (Rect.toSKRect rect)
    liftIO $
        sk_canvas_clip_rect_with_operation
            canvas'
            rect'
            (marshalSKEnum op)
            (fromBool doAA)

clipPathWithOp ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    SKPath ->
    SKClipOp ->
    -- | Do antialiasing?
    Bool ->
    m ()
clipPathWithOp (toA SKCanvas -> canvas) path op doAA = evalContIO do
    canvas' <- useObj canvas
    path' <- useObj path
    liftIO $
        sk_canvas_clip_path_with_operation
            canvas'
            path'
            (marshalSKEnum op)
            (fromBool doAA)

clipRoundRectWithOp ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    SKRoundRect ->
    SKClipOp ->
    -- | Do antialiasing?
    Bool ->
    m ()
clipRoundRectWithOp (toA SKCanvas -> canvas) rrect op doAA = evalContIO do
    canvas' <- useObj canvas
    rrect' <- useObj rrect
    liftIO $
        sk_canvas_clip_rrect_with_operation
            canvas'
            rrect'
            (marshalSKEnum op)
            (fromBool doAA)

-- | Returns 'Nothing' if the bounds if empty.
getLocalClipBounds :: (MonadIO m, IsSKCanvas canvas) => canvas -> m (Maybe (Rect Float))
getLocalClipBounds (toA SKCanvas -> canvas) = evalContIO do
    canvas' <- useObj canvas
    bounds' <- useAlloca
    isempty <- liftIO $ fmap toBool $ sk_canvas_get_local_clip_bounds canvas' bounds'
    if isempty
        then pure Nothing
        else Just <$> peekWith fromSKRect bounds'

getDeviceClipBounds :: (MonadIO m, IsSKCanvas canvas) => canvas -> m (Maybe (Rect Int))
getDeviceClipBounds (toA SKCanvas -> canvas) = evalContIO do
    canvas' <- useObj canvas
    bounds' <- useAlloca
    isempty <- liftIO $ fmap toBool $ sk_canvas_get_device_clip_bounds canvas' bounds'
    if isempty
        then pure Nothing
        else Just <$> peekWith fromSKIRect bounds'

save :: (MonadIO m, IsSKCanvas canvas) => canvas -> m StackDepth
save (toA SKCanvas -> canvas) = evalContIO do
    canvas' <- useObj canvas
    liftIO $ fmap fromIntegral $ sk_canvas_save canvas'

saveLayer :: (MonadIO m, IsSKCanvas canvas) => canvas -> Rect Float -> SKPaint -> m StackDepth
saveLayer (toA SKCanvas -> canvas) rect paint = evalContIO do
    canvas' <- useObj canvas
    rect' <- useStorable (Rect.toSKRect rect)
    paint' <- useObj paint
    liftIO $ fmap fromIntegral $ sk_canvas_save_layer canvas' rect' paint'

saveLayerRec :: (MonadIO m, IsSKCanvas canvas) => canvas -> SKCanvasSaveLayerRec -> m StackDepth
saveLayerRec (toA SKCanvas -> canvas) savelayer = evalContIO do
    canvas' <- useObj canvas
    savelayer' <- useObj savelayer
    liftIO $ fmap fromIntegral $ sk_canvas_save_layer_rec canvas' savelayer'

restore :: (MonadIO m, IsSKCanvas canvas) => canvas -> m ()
restore (toA SKCanvas -> canvas) = evalContIO do
    canvas' <- useObj canvas
    liftIO $ sk_canvas_restore canvas'

translate :: (MonadIO m, IsSKCanvas canvas) => canvas -> V2 Float -> m ()
translate (toA SKCanvas -> canvas) (V2 x y) = evalContIO do
    canvas' <- useObj canvas
    liftIO $ sk_canvas_translate canvas' (coerce x) (coerce y)

scale :: (MonadIO m, IsSKCanvas canvas) => canvas -> V2 Float -> m ()
scale (toA SKCanvas -> canvas) (V2 x y) = evalContIO do
    canvas' <- useObj canvas
    liftIO $ sk_canvas_scale canvas' (coerce x) (coerce y)

rotateDegrees :: (MonadIO m, IsSKCanvas canvas) => canvas -> Degrees -> m ()
rotateDegrees (toA SKCanvas -> canvas) degrees = evalContIO do
    canvas' <- useObj canvas
    liftIO $ sk_canvas_rotate_degrees canvas' (coerce degrees)

rotate :: (MonadIO m, IsSKCanvas canvas) => canvas -> Radians -> m ()
rotate (toA SKCanvas -> canvas) radians = evalContIO do
    canvas' <- useObj canvas
    liftIO $ sk_canvas_rotate_radians canvas' (coerce radians)

skew :: (MonadIO m, IsSKCanvas canvas) => canvas -> V2 Float -> m ()
skew (toA SKCanvas -> canvas) (V2 x y) = evalContIO do
    canvas' <- useObj canvas
    liftIO $ sk_canvas_skew canvas' (coerce x) (coerce y)

concat :: (MonadIO m, IsSKCanvas canvas) => canvas -> M44 Float -> m ()
concat (toA SKCanvas -> canvas) matrix = evalContIO do
    canvas' <- useObj canvas
    matrix' <- useStorable (toSKMatrix44 matrix)
    liftIO $ sk_canvas_concat canvas' matrix'

quickReject :: (MonadIO m, IsSKCanvas canvas) => canvas -> Rect Float -> m Bool
quickReject (toA SKCanvas -> canvas) rect = evalContIO do
    canvas' <- useObj canvas
    rect' <- useStorable (Rect.toSKRect rect)
    liftIO $ fmap toBool $ sk_canvas_quick_reject canvas' rect'

clipRegion :: (MonadIO m, IsSKCanvas canvas) => canvas -> SKRegion -> SKClipOp -> m ()
clipRegion (toA SKCanvas -> canvas) region op = evalContIO do
    canvas' <- useObj canvas
    region' <- useObj region
    liftIO $ sk_canvas_clip_region canvas' region' (marshalSKEnum op)

drawPaint :: (MonadIO m, IsSKCanvas canvas) => canvas -> SKPaint -> m ()
drawPaint (toA SKCanvas -> canvas) paint = evalContIO do
    canvas' <- useObj canvas
    paint' <- useObj paint
    liftIO $ sk_canvas_draw_paint canvas' paint'

drawRegion :: (MonadIO m, IsSKCanvas canvas) => canvas -> SKRegion -> SKPaint -> m ()
drawRegion (toA SKCanvas -> canvas) region paint = evalContIO do
    canvas' <- useObj canvas
    region' <- useObj region
    paint' <- useObj paint
    liftIO $ sk_canvas_draw_region canvas' region' paint'

drawRect :: (MonadIO m, IsSKCanvas canvas) => canvas -> Rect Float -> SKPaint -> m ()
drawRect (toA SKCanvas -> canvas) rect paint = evalContIO do
    canvas' <- useObj canvas
    rect' <- useStorable (Rect.toSKRect rect)
    paint' <- useObj paint
    liftIO $ sk_canvas_draw_rect canvas' rect' paint'

drawRoundRect :: (MonadIO m, IsSKCanvas canvas) => canvas -> SKRoundRect -> SKPaint -> m ()
drawRoundRect (toA SKCanvas -> canvas) rrect paint = evalContIO do
    canvas' <- useObj canvas
    rrect' <- useObj rrect
    paint' <- useObj paint
    liftIO $ sk_canvas_draw_rrect canvas' rrect' paint'

drawCircle ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | Center
    V2 Float ->
    -- | Radius
    Float ->
    SKPaint ->
    m ()
drawCircle (toA SKCanvas -> canvas) (V2 cx cy) r paint = evalContIO do
    canvas' <- useObj canvas
    paint' <- useObj paint
    liftIO $
        sk_canvas_draw_circle
            canvas'
            (coerce cx)
            (coerce cy)
            (coerce r)
            paint'

drawOval ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    Rect Float ->
    SKPaint ->
    m ()
drawOval (toA SKCanvas -> canvas) rect paint = evalContIO do
    canvas' <- useObj canvas
    rect' <- useStorable (Rect.toSKRect rect)
    paint' <- useObj paint
    liftIO $ sk_canvas_draw_oval canvas' rect' paint'

drawPath ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    SKPath ->
    SKPaint ->
    m ()
drawPath (toA SKCanvas -> canvas) path paint = evalContIO do
    canvas' <- useObj canvas
    path' <- useObj path
    paint' <- useObj paint
    liftIO $ sk_canvas_draw_path canvas' path' paint'

drawImage :: (MonadIO m, IsSKCanvas canvas) => canvas -> SKImage -> V2 Float -> SKSamplingOptions -> SKPaint -> m ()
drawImage (toA SKCanvas -> canvas) im (V2 x y) sampling paint = evalContIO do
    canvas' <- useObj canvas
    im' <- useObj im
    sampling' <- useStorable (marshalSKSamplingOptions sampling)
    paint' <- useObj paint
    liftIO $ sk_canvas_draw_image canvas' im' (coerce x) (coerce y) sampling' paint'

drawImageRect ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    SKImage ->
    -- | Source bounds
    Rect Float ->
    -- | Destination bounds
    Rect Float ->
    SKSamplingOptions ->
    SKPaint ->
    m ()
drawImageRect (toA SKCanvas -> canvas) im srcRect dstRect sampling paint = evalContIO do
    canvas' <- useObj canvas
    im' <- useObj im
    srcRect' <- useStorable (Rect.toSKRect srcRect)
    dstRect' <- useStorable (Rect.toSKRect dstRect)
    sampling' <- useStorable (marshalSKSamplingOptions sampling)
    paint' <- useObj paint
    liftIO $ sk_canvas_draw_image_rect canvas' im' srcRect' dstRect' sampling' paint'

drawPicture :: (MonadIO m, IsSKCanvas canvas) => canvas -> SKPicture -> M33 Float -> SKPaint -> m ()
drawPicture (toA SKCanvas -> canvas) picture matrix paint = evalContIO do
    canvas' <- useObj canvas
    picture' <- useObj picture
    matrix' <- useStorable (toSKMatrix matrix)
    paint' <- useObj paint
    liftIO $ sk_canvas_draw_picture canvas' picture' matrix' paint'

drawDrawable :: (MonadIO m, IsSKCanvas canvas) => canvas -> SKDrawable -> M33 Float -> m ()
drawDrawable (toA SKCanvas -> canvas) drawable matrix = evalContIO do
    canvas' <- useObj canvas
    drawable' <- useObj drawable
    matrix' <- useStorable (toSKMatrix matrix)
    liftIO $ sk_canvas_draw_drawable canvas' drawable' matrix'

createFromBitmap :: (MonadIO m) => SKBitmap -> m SKCanvas
createFromBitmap bitmap = evalContIO do
    bitmap' <- useObj bitmap
    liftIO $ toObject =<< sk_canvas_new_from_bitmap bitmap'

createFromRaster ::
    (MonadIO m) =>
    SKImageInfo ->
    -- | Pixel buffer
    Ptr Word8 ->
    -- | Row bytes
    Int ->
    SKSurfaceProps ->
    m SKCanvas
createFromRaster iminfo pixelBuffer rowBytes surfaceProps = evalContIO do
    iminfo' <- useSKImageInfo iminfo
    surfaceProps' <- useSKSurfaceProps surfaceProps
    liftIO $
        toObject
            =<< sk_canvas_new_from_raster
                iminfo'
                (castPtr pixelBuffer)
                (fromIntegral rowBytes)
                surfaceProps'

drawAnnotationRaw ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    Rect Float ->
    -- | Key
    CString ->
    -- | Value
    SKData ->
    m ()
drawAnnotationRaw (toA SKCanvas -> canvas) rect key value = evalContIO do
    canvas' <- useObj canvas
    rect' <- useStorable (Rect.toSKRect rect)
    value' <- useObj value
    liftIO $
        sk_canvas_draw_annotation
            canvas'
            rect'
            key
            value'

drawUrlAnnotation ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    Rect Float ->
    -- | Value
    SKData ->
    m ()
drawUrlAnnotation (toA SKCanvas -> canvas) rect value = evalContIO do
    canvas' <- useObj canvas
    rect' <- useStorable (Rect.toSKRect rect)
    value' <- useObj value
    liftIO $ sk_canvas_draw_url_annotation canvas' rect' value'

drawNamedDestinationAnnotation ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | Point
    V2 Float ->
    -- | Value
    SKData ->
    m ()
drawNamedDestinationAnnotation (toA SKCanvas -> canvas) point value = evalContIO do
    canvas' <- useObj canvas
    point' <- useStorable (toSKPoint point)
    value' <- useObj value
    liftIO $ sk_canvas_draw_named_destination_annotation canvas' point' value'

drawLinkDestinationAnnotation ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | Rect
    Rect Float ->
    -- | Value
    SKData ->
    m ()
drawLinkDestinationAnnotation (toA SKCanvas -> canvas) rect value = evalContIO do
    canvas' <- useObj canvas
    rect' <- useStorable (Rect.toSKRect rect)
    value' <- useObj value
    liftIO $ sk_canvas_draw_link_destination_annotation canvas' rect' value'

drawImageLatticeRaw ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    SKImage ->
    Sk_lattice ->
    -- | Destination
    Rect Float ->
    SKFilterMode ->
    SKPaint ->
    m ()
drawImageLatticeRaw (toA SKCanvas -> canvas) image lattice dst filterMode paint = evalContIO do
    canvas' <- useObj canvas
    image' <- useObj image
    lattice' <- useStorable lattice
    dst' <- useStorable (Rect.toSKRect dst)
    paint' <- useObj paint
    liftIO $ sk_canvas_draw_image_lattice canvas' image' lattice' dst' (marshalSKEnum filterMode) paint'

drawImageNine ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    SKImage ->
    -- | Center
    Rect Int ->
    -- | Destination
    Rect Float ->
    SKFilterMode ->
    SKPaint ->
    m ()
drawImageNine (toA SKCanvas -> canvas) image center dst filterMode paint = evalContIO do
    canvas' <- useObj canvas
    image' <- useObj image
    center' <- useStorable (Rect.toSKIRect center)
    dst' <- useStorable (Rect.toSKRect dst)
    paint' <- useObj paint
    liftIO $ sk_canvas_draw_image_nine canvas' image' center' dst' (marshalSKEnum filterMode) paint'

drawVertices :: (MonadIO m, IsSKCanvas canvas) => canvas -> SKVertices -> SKBlendMode -> SKPaint -> m ()
drawVertices (toA SKCanvas -> canvas) vertices blendMode paint = evalContIO do
    canvas' <- useObj canvas
    vertices' <- useObj vertices
    paint' <- useObj paint
    liftIO $ sk_canvas_draw_vertices canvas' vertices' (marshalSKEnum blendMode) paint'

drawArc ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | Oval
    Rect Float ->
    -- | Start angle
    Float ->
    -- | Sweep angle
    Float ->
    -- | Use center
    Bool ->
    SKPaint ->
    m ()
drawArc (toA SKCanvas -> canvas) oval startAngle sweepAngle useCenter paint = evalContIO do
    canvas' <- useObj canvas
    oval' <- useStorable (Rect.toSKRect oval)
    paint' <- useObj paint
    liftIO $
        sk_canvas_draw_arc
            canvas'
            oval'
            (coerce startAngle)
            (coerce sweepAngle)
            (fromBool useCenter)
            paint'

drawDoubleRRect ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | Outer
    SKRoundRect ->
    -- | Inner
    SKRoundRect ->
    SKPaint ->
    m ()
drawDoubleRRect (toA SKCanvas -> canvas) outer inner paint = evalContIO do
    canvas' <- useObj canvas
    outer' <- useObj outer
    inner' <- useObj inner
    paint' <- useObj paint
    liftIO $ sk_canvas_draw_drrect canvas' outer' inner' paint'

drawAtlasRaw ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | Atlas
    SKImage ->
    Sk_rsxform ->
    -- | Tex
    Rect Float ->
    -- | Colors
    Ptr SKColor ->
    -- | Colors count
    Int ->
    SKBlendMode ->
    SKSamplingOptions ->
    -- | Cull rect
    Rect Float ->
    SKPaint ->
    m ()
drawAtlasRaw (toA SKCanvas -> canvas) atlas xform tex colors count mode sampling cullRect paint = evalContIO do
    canvas' <- useObj canvas
    atlas' <- useObj atlas
    xform' <- useStorable xform
    tex' <- useStorable (Rect.toSKRect tex)
    sampling' <- useStorable (marshalSKSamplingOptions sampling)
    cullRect' <- useStorable (Rect.toSKRect cullRect)
    paint' <- useObj paint
    liftIO $
        sk_canvas_draw_atlas
            canvas'
            atlas'
            xform'
            tex'
            (coercePtr colors)
            (fromIntegral count)
            (marshalSKEnum mode)
            sampling'
            cullRect'
            paint'

drawPatchRaw ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | Cubics
    Ptr Sk_point ->
    -- | Colors
    Ptr Sk_color ->
    -- | Tex coords
    Ptr Sk_point ->
    SKBlendMode ->
    SKPaint ->
    m ()
drawPatchRaw (toA SKCanvas -> canvas) cubics colors texCoords blendMode paint = evalContIO do
    canvas' <- useObj canvas
    paint' <- useObj paint
    liftIO $
        sk_canvas_draw_patch
            canvas'
            cubics
            colors
            texCoords
            (marshalSKEnum blendMode)
            paint'

isClipEmpty :: (MonadIO m, IsSKCanvas canvas) => canvas -> m Bool
isClipEmpty (toA SKCanvas -> canvas) = evalContIO do
    canvas' <- useObj canvas
    liftIO $ fmap toBool $ sk_canvas_is_clip_empty canvas'

isClipRect :: (MonadIO m, IsSKCanvas canvas) => canvas -> m Bool
isClipRect (toA SKCanvas -> canvas) = evalContIO do
    canvas' <- useObj canvas
    liftIO $ fmap toBool $ sk_canvas_is_clip_rect canvas'

-- | Returns Ganesh context of the GPU surface associated with SkCanvas.
getRecordingContext ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | GPU context, if available; 'Nothing' otherwise
    m (Maybe GRRecordingContext)
getRecordingContext (toA SKCanvas -> canvas) = evalContIO do
    canvas' <- useObj canvas
    ctx' <- liftIO $ sk_get_recording_context canvas'
    if ctx' == nullPtr
        then pure Nothing
        else do
            -- TODO: Unref? Need to confirm
            Just <$> toObject ctx'

{- | Sometimes a canvas is owned by a surface. If it is, this function will
return surface directly owned by the canvas, else this will return 'Nothing'.
-}
getSurface :: (MonadIO m, IsSKCanvas canvas) => canvas -> m (Maybe SKSurface)
getSurface (toA SKCanvas -> canvas) = evalContIO do
    canvas' <- useObj canvas
    surface' <- liftIO $ sk_get_surface canvas'
    if surface' == nullPtr
        then pure Nothing
        else Just <$> toObject surface'
