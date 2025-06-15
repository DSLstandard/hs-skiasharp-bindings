module Skia.SKCanvas (
    -- * Creating 'SKCanvas'
    createFromBitmap,
    createFromRaster,

    -- * Canvas state @save()@ and @restore()@
    StackDepth (..),
    save,
    saveLayer,
    SaveLayerRecFlags (..),
    SaveLayerRec (..),
    saveLayerRec,
    restore,
    getSaveCount,
    restoreToCount,

    -- * Transform operations
    translate,
    scale,
    rotate,
    skew,
    concat,
    setMatrix,
    resetMatrix,

    -- * Clip operations
    clipRect,
    clipRRect,
    clipPath,
    clipRegion,
    quickReject,
    getLocalClipBounds,
    getDeviceClipBounds,
    isClipEmpty,
    isClipRect,
    getLocalToDevice,

    -- * Drawing operations
    drawColor,
    drawColorRGBA,
    clear,
    clearRGBA,
    discard,
    drawPaint,
    drawPoints,
    drawPoint,
    drawLine,
    drawRect,
    drawRegion,
    drawOval,
    drawRRect,
    drawDRRect,
    drawCircle,
    drawArc,
    drawRoundRect,
    drawPath,
    drawImage,
    drawImageRect,
    drawImageLattice,
    drawSimpleTextEncoding,
    drawSimpleText,
    drawTextBlob,
    drawPicture,
    drawVertices,
    drawPatch,
    drawAtlas,
    drawDrawable,
    drawAnnotation,
    drawUrlAnnotation,
    drawNamedDestinationAnnotation,
    drawLinkDestinationAnnotation,

    -- * Getters
    getSurface,
    getRecordingContext,
)
where

import Control.Exception
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BS
import Data.List qualified
import Data.Maybe qualified
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector.Storable qualified as VS
import Linear hiding (rotate)
import Skia.Types.Rect qualified as Rect
import Prelude hiding (concat)

-- Hiding Data.Bits.rotate
import Skia.Internal.Prelude hiding (rotate)

{- | Constructs a canvas that draws into bitmap. Sets kUnknown_SkPixelGeometry
in constructed SkSurface.

SkBitmap is copied so that subsequently editing bitmap will not affect
constructed SkCanvas.

When 'Acquire' releases, the returned 'SKCanvas' is destroyed with 'sk_canvas_destroy'.

**Comment from Google Skia: May be deprecated in the future.**
-}
createFromBitmap :: (MonadIO m) => SKBitmap -> Acquire SKCanvas
createFromBitmap bitmap =
    mkSKObjectAcquire
        (sk_canvas_new_from_bitmap (ptr bitmap))
        sk_canvas_destroy

{- | Allocates raster SkCanvas that will draw directly into pixels.

SkCanvas is returned if all parameters are valid. Valid parameters include: info
dimensions are zero or positive; info contains SkColorType and SkAlphaType
supported by raster surface; pixels is not nullptr; rowBytes is zero or large
enough to contain info width pixels of SkColorType.

Pass 'Nothing' for rowBytes to compute rowBytes from info width and size of
pixel. If rowBytes is 'Just', it must be equal to or greater than info width
times bytes required for SkColorType.

Pixel buffer size should be info height times computed rowBytes. Pixels are not
initialized. To access pixels after drawing, call flush() or peekPixels().

When 'Acquire' releases, the returned 'SKCanvas' is destroyed with
'sk_canvas_destroy'.
-}
createFromRaster ::
    -- | width, height, SkColorType, SkAlphaType, SkColorSpace, of raster
    -- surface;
    --
    -- width, or height, or both, may be zero
    SKImageInfo ->
    -- | pixels. Pointer to destination pixels buffer
    Ptr Word8 ->
    -- | rowBytes. Optional. Interval from one SkSurface row to the next.
    Maybe Int ->
    -- | props. Optional. LCD striping orientation and setting for device independent fonts;
    Maybe SKSurfaceProps ->
    Acquire SKCanvas
createFromRaster iminfo pixelBuffer rowBytes surfaceProps =
    mkSKObjectAcquire
        ( evalContIO do
            iminfo' <- useSKImageInfo iminfo
            surfaceProps' <- useNullIfNothing useSKSurfaceProps surfaceProps
            liftIO $ sk_canvas_new_from_raster iminfo' (castPtr pixelBuffer) (maybe 0 fromIntegral rowBytes) surfaceProps'
        )
        sk_canvas_destroy

newtype StackDepth = StackDepth {unStackDepth :: Int}
    deriving (Show, Eq, Ord)
    deriving newtype (Num)

{- | Saves the current transform and clip.

Calling 'restore' discards changes to the current transform and clip, restoring
the current transform and clip to their state when 'save' was called.

The current transform may be changed by 'translate', 'scale', 'rotate', 'skew',
'concat', 'setMatrix', and 'resetMatrix'. Clip may be changed by 'clipRect',
'clipRRect', 'clipPath', 'clipRegion'.

Saved 'SKCanvas' state is put on a stack; multiple calls to 'save' should be
balance by an equal number of calls to 'restore'.

Call 'restoreToCount' with result to restore this and subsequent saves.
-}
save :: (MonadIO m, IsSKCanvas canvas) => canvas -> m StackDepth
save (toA SKCanvas -> canvas) = evalContIO do
    canvas' <- useObj canvas
    liftIO $ fmap fromIntegral $ sk_canvas_save canvas'

{- | Saves the current transform and clip, and allocates a 'SKSurface' for
subsequent drawing. Calling 'restore' discards changes to the transform and
clip, and draws the 'SKSurface'.

The current transform may be changed by 'translate', 'scale', 'rotate', 'skew',
'concat', 'setMatrix', and 'resetMatrix'.

Clip may be changed by 'clipRect', 'clipRRect', 'clipPath', 'clipRegion'.

\"bounds\" suggests but does not define the 'SKSurface' size. To clip drawing to
a specific rectangle, use 'clipRect'.

Optional 'SKPaint' paint applies alpha, SkColorFilter, SkImageFilter, and
SkBlendMode when 'restore' is called.

Call 'restoreToCount' with returned value to restore this and subsequent saves.
-}
saveLayer ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | bounds. Optional. Hint to limit the size of the layer
    Maybe (Rect Float) ->
    -- | paint. Optional. Graphics state for layer
    Maybe SKPaint ->
    m StackDepth
saveLayer (toA SKCanvas -> canvas) bounds paint = evalContIO do
    bounds' <- useNullIfNothing useStorable $ fmap toSKRect $ bounds
    liftIO $
        fmap fromIntegral $
            sk_canvas_save_layer
                (ptr canvas)
                bounds'
                (ptrOrNull paint)

-- | Auxillary structure in 'SaveLayerRec'.
data SaveLayerRecFlags = SaveLayerRecFlags
    { preserveLcdText :: Bool
    , initializeWithPrevious :: Bool
    -- ^ Initializes with previous contents?
    , useF16ColorType :: Bool
    -- ^ instead of matching previous layer's colortype, use F16
    }
    deriving (Show, Eq, Ord)

marshalSaveLayerRecFlags :: SaveLayerRecFlags -> Sk_canvas_savelayerrec_flags
marshalSaveLayerRecFlags i =
    makeBitFlags
        [ (i.preserveLcdText, PRESERVE_LCD_TEXT_SK_CANVAS_SAVELAYERREC_FLAGS)
        , (i.initializeWithPrevious, INITIALIZE_WITH_PREVIOUS_SK_CANVAS_SAVELAYERREC_FLAGS)
        , (i.useF16ColorType, F16_COLOR_TYPE_SK_CANVAS_SAVELAYERREC_FLAGS)
        ]

{- | 'SaveLayerRec' contains the state used to create a layer in 'SKCanvas'.

See function 'saveLayerRec'.
-}
data SaveLayerRec = SaveLayerRec
    { bounds :: Maybe (Rect Float)
    -- ^ Optional. Hint to limit the size of the layer
    , paint :: Maybe SKPaint
    -- ^ Optional. Modifies overlay.
    , backdrop :: Maybe SKImageFilter
    -- ^ If not 'Nothing', this triggers the same initialization behavior as
    -- setting kInitWithPrevious_SaveLayerFlag on fSaveLayerFlags: the current
    -- layer is copied into the new layer, rather than initializing the new
    -- layer with transparent-black. This is then filtered by fBackdrop
    -- (respecting the current clip).
    , preserveLCDText :: Bool
    -- ^ Preserves LCD text, creates with prior layer contents
    , flags :: SaveLayerRecFlags
    }

useSaveLayerRec :: SaveLayerRec -> ContT r IO Sk_canvas_savelayerrec
useSaveLayerRec i = evalContIO do
    fBounds <- useNullIfNothing useStorable $ fmap toSKRect i.bounds
    pure
        Sk_canvas_savelayerrec
            { fBounds
            , fPaint = ptrOrNull i.paint
            , fBackdrop = ptrOrNull i.backdrop
            , fFlags = marshalSaveLayerRecFlags i.flags
            }

{- | Saves the current transform and clip, and allocates 'SKSurface' for
subsequent drawing.

Calling 'restore' discards changes to the current transofrm and clip, and blends
'SKSurface' with alpha opacity onto the prior layer.

The current transform may be changed by 'translate', 'scale', 'rotate', 'skew',
'concat', 'setMatrix', and 'resetMatrix'. Clip may be changed by 'clipRect',
'clipRRect', 'clipPath', 'clipRegion'.

The input SaveLayerRec contains the state used to create the layer.

Call 'restoreToCount' with returned value to restore this and subsequent saves.
-}
saveLayerRec ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | Layer state
    SaveLayerRec ->
    m StackDepth
saveLayerRec (toA SKCanvas -> canvas) savelayer = evalContIO do
    savelayer' <- useStorable =<< useSaveLayerRec savelayer
    liftIO $ fmap fromIntegral $ sk_canvas_save_layer_rec (ptr canvas) savelayer'

restore :: (MonadIO m, IsSKCanvas canvas) => canvas -> m ()
restore (toA SKCanvas -> canvas) = evalContIO do
    canvas' <- useObj canvas
    liftIO $ sk_canvas_restore canvas'

{- | Returns the number of saved states, each containing: SkMatrix and clip.
Equals the number of 'save' calls less the number of restore() calls plus
one. The save count of a new canvas is one.
-}
getSaveCount :: (MonadIO m, IsSKCanvas canvas) => canvas -> m StackDepth
getSaveCount (toA SKCanvas -> canvas) = liftIO do
    fmap fromIntegral $ sk_canvas_get_save_count (ptr canvas)

restoreToCount :: (MonadIO m, IsSKCanvas canvas) => canvas -> StackDepth -> m ()
restoreToCount (toA SKCanvas -> canvas) (StackDepth saveCount) = liftIO do
    sk_canvas_restore_to_count (ptr canvas) (fromIntegral saveCount)

-- | Translates the current transform by (dx, dy).
translate ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | (dx, dy)
    V2 Float ->
    m ()
translate (toA SKCanvas -> canvas) (V2 x y) = liftIO do
    sk_canvas_translate (ptr canvas) (coerce x) (coerce y)

-- | Scales the current transform by (sx, sy).
scale ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | (sx, sy)
    V2 Float ->
    m ()
scale (toA SKCanvas -> canvas) (V2 x y) = liftIO do
    sk_canvas_scale (ptr canvas) (coerce x) (coerce y)

{- | Rotates the current transform by degrees. Positive degrees rotates
clockwise.
-}
rotate ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | degrees
    Degrees ->
    m ()
rotate (toA SKCanvas -> canvas) degrees = liftIO do
    liftIO $ sk_canvas_rotate_degrees (ptr canvas) (coerce degrees)

{- | Skews the current transform by sx on the x-axis and sy on the y-axis. A
positive value of sx skews the drawing right as y-axis values increase; a
positive value of sy skews the drawing down as x-axis values increase.
-}
skew :: (MonadIO m, IsSKCanvas canvas) => canvas -> V2 Float -> m ()
skew (toA SKCanvas -> canvas) (V2 x y) = liftIO do
    liftIO $ sk_canvas_skew (ptr canvas) (coerce x) (coerce y)

{- | Premultiplies the input 4x4 matrix into the current transform.

TIP: If you only have a 'Linear.M33', consider using 'Linear.m33_to_m44'.
-}
concat :: (MonadIO m, IsSKCanvas canvas) => canvas -> M44 Float -> m ()
concat (toA SKCanvas -> canvas) matrix = evalContIO do
    matrix' <- useStorable (toSKMatrix44 matrix)
    liftIO $ sk_canvas_concat (ptr canvas) matrix'

{- | Replaces the current transform with the input 4x4 matrix.

Unlike 'concat', any prior matrix state is overwritten.

TIP: If you only have a 'Linear.M33', consider using 'Linear.m33_to_m44'.
-}
setMatrix :: (MonadIO m, IsSKCanvas canvas) => canvas -> M44 Float -> m ()
setMatrix (toA SKCanvas -> canvas) matrix = evalContIO do
    matrix' <- useStorable (toSKMatrix44 matrix)
    liftIO $ sk_canvas_set_matrix (ptr canvas) matrix'

{- | Sets the current transform to the identity matrix.

Any prior matrix state is overwritten.
-}
resetMatrix :: (MonadIO m, IsSKCanvas canvas) => canvas -> m ()
resetMatrix (toA SKCanvas -> canvas) = liftIO do
    sk_canvas_reset_matrix (ptr canvas)

{- | Replaces clip with the intersection or difference of clip and rect, with an
aliased or anti-aliased clip edge.

\"rect\" is transformed by the current transform before it is combined with clip.
-}
clipRect ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | \"rect\". 'Rect' to combine with clip.
    Rect Float ->
    -- | Operation to apply to clip
    SKClipOp ->
    -- | true if clip is to be anti-aliased
    Bool ->
    m ()
clipRect (toA SKCanvas -> canvas) rect op doAA = evalContIO do
    canvas' <- useObj canvas
    rect' <- useStorable (toSKRect rect)
    liftIO $
        sk_canvas_clip_rect_with_operation
            canvas'
            rect'
            (marshalSKEnum op)
            (fromBool doAA)

{- | Replaces clip with the intersection or difference of clip and rrect, with
an aliased or anti-aliased clip edge. rrect is transformed by SkMatrix before
it is combined with clip.
-}
clipRRect ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | rrect. SkRRect to combine with clip
    SKRRect ->
    -- | op. SkClipOp to apply to clip
    SKClipOp ->
    -- | doAntialias. true if clip is to be anti-aliased
    Bool ->
    m ()
clipRRect (toA SKCanvas -> canvas) rrect op doAA = evalContIO do
    canvas' <- useObj canvas
    rrect' <- useObj rrect
    liftIO $
        sk_canvas_clip_rrect_with_operation
            canvas'
            rrect'
            (marshalSKEnum op)
            (fromBool doAA)

{- | Replaces clip with the intersection or difference of clip and path, with an
aliased or anti-aliased clip edge.

The 'Skia.SKPath.getFillType' of the 'SKPath' determines if path describes the
area inside or outside its contours; and if path contour overlaps itself or
another path contour, whether the overlaps form part of the area. path is
transformed by the current transform before it is combined with clip.
-}
clipPath ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | 'SKPath' to combine with clip
    SKPath ->
    -- | Operation to apply to clip
    SKClipOp ->
    -- | true if clip is to be anti-aliased
    Bool ->
    m ()
clipPath (toA SKCanvas -> canvas) path op doAA = evalContIO do
    canvas' <- useObj canvas
    path' <- useObj path
    liftIO $
        sk_canvas_clip_path_with_operation
            canvas'
            path'
            (marshalSKEnum op)
            (fromBool doAA)

{- | Replaces clip with the intersection or difference of clip and SkRegion
deviceRgn. Resulting clip is aliased; pixels are fully contained by the clip.
deviceRgn is unaffected by SkMatrix.
-}
clipRegion ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | deviceRgn. SkRegion to combine with clip
    SKRegion ->
    -- | SkClipOp to apply to clip
    SKClipOp ->
    m ()
clipRegion (toA SKCanvas -> canvas) region op = evalContIO do
    canvas' <- useObj canvas
    region' <- useObj region
    liftIO $ sk_canvas_clip_region canvas' region' (marshalSKEnum op)

{- | Returns true if SkRect rect, transformed by SkMatrix, can be quickly determined to be
outside of clip. May return false even though rect is outside of clip.

Use to check if an area to be drawn is clipped out, to skip subsequent draw calls.
-}
quickReject ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | SkRect to compare with clip
    Rect Float ->
    m Bool
quickReject (toA SKCanvas -> canvas) rect = evalContIO do
    canvas' <- useObj canvas
    rect' <- useStorable (toSKRect rect)
    liftIO $ fmap toBool $ sk_canvas_quick_reject canvas' rect'

{- | Returns bounds of clip, transformed by inverse of SkMatrix. If clip is
empty, return 'Nothing'..

The returned bounds is outset by one to account for partial pixel coverage if
clip is anti-aliased.
-}
getLocalClipBounds :: (MonadIO m, IsSKCanvas canvas) => canvas -> m (Maybe (Rect Float))
getLocalClipBounds (toA SKCanvas -> canvas) = evalContIO do
    canvas' <- useObj canvas
    bounds' <- useAlloca
    isempty <- liftIO $ fmap toBool $ sk_canvas_get_local_clip_bounds canvas' bounds'
    if isempty
        then pure Nothing
        else Just <$> peekWith fromSKRect bounds'

{- | Returns SkIRect bounds of clip, unaffected by SkMatrix. If clip is empty,
return 'Nothing'.

Unlike 'getLocalClipBounds', returned SkIRect is not outset.
-}
getDeviceClipBounds :: (MonadIO m, IsSKCanvas canvas) => canvas -> m (Maybe (Rect Int))
getDeviceClipBounds (toA SKCanvas -> canvas) = evalContIO do
    canvas' <- useObj canvas
    bounds' <- useAlloca
    isempty <- liftIO $ fmap toBool $ sk_canvas_get_device_clip_bounds canvas' bounds'
    if isempty
        then pure Nothing
        else Just <$> peekWith fromSKIRect bounds'

{- | Fills clip with color color. Input blend mode determines how ARGB is
combined with destination.
-}
drawColor :: (MonadIO m, IsSKCanvas canvas) => canvas -> SKColor -> SKBlendMode -> m ()
drawColor (toA SKCanvas -> canvas) hex blendMode = evalContIO do
    canvas' <- useObj canvas
    liftIO $ sk_canvas_draw_color canvas' (coerce hex) (marshalSKEnum blendMode)

-- | Like 'drawColor' but accepts 'RGBA'.
drawColorRGBA :: (MonadIO m, IsSKCanvas canvas) => canvas -> RGBA Float -> SKBlendMode -> m ()
drawColorRGBA (toA SKCanvas -> canvas) color blendMode = evalContIO do
    canvas' <- useObj canvas
    liftIO $ sk_canvas_draw_color4f canvas' (toSKColor4f color) (marshalSKEnum blendMode)

{- | Fills clip with the input color using 'SKBlendMode'Src'. This has the
effect of replacing all pixels contained by clip with color.
-}
clear :: (MonadIO m, IsSKCanvas canvas) => canvas -> SKColor -> m ()
clear (toA SKCanvas -> canvas) hex = evalContIO do
    canvas' <- useObj canvas
    liftIO $ sk_canvas_clear canvas' (coerce hex)

-- | Like 'clear' but takes in 'RGBA'.
clearRGBA :: (MonadIO m, IsSKCanvas canvas) => canvas -> RGBA Float -> m ()
clearRGBA (toA SKCanvas -> canvas) color = evalContIO do
    canvas' <- useObj canvas
    liftIO $ sk_canvas_clear_color4f canvas' (toSKColor4f color)

{- | Makes 'SKCanvas' contents undefined. Subsequent calls that read 'SKCanvas'
pixels, such as drawing with 'SKBlendMode', return undefined results. 'discard'
does not change clip or the transform.

'discard' may do nothing, depending on the implementation of 'SKSurface' or the
underlying Skia @SkDevice@ that created 'SKCanvas'.

'discard' allows optimized performance on subsequent draws by removing cached
data associated with 'SKSurface' or the underlying Skia @SkDevice@. It is not
necessary to call 'discard' once done with 'SKCanvas'; any cached data is
deleted when owning 'SKSurface' or the underlying Skia @SkDevice@ is deleted.
-}
discard :: (MonadIO m, IsSKCanvas canvas) => canvas -> m ()
discard (toA SKCanvas -> canvas) = evalContIO do
    canvas' <- useObj canvas
    liftIO $ sk_canvas_discard canvas'

{- | Fills clip with SkPaint paint. SkPaint components, SkShader, SkColorFilter,
SkImageFilter, and SkBlendMode affect drawing; SkMaskFilter and SkPathEffect
in paint are ignored.
-}
drawPaint :: (MonadIO m, IsSKCanvas canvas) => canvas -> SKPaint -> m ()
drawPaint (toA SKCanvas -> canvas) paint = evalContIO do
    canvas' <- useObj canvas
    paint' <- useObj paint
    liftIO $ sk_canvas_draw_paint canvas' paint'

{- | Draws the input points using clip, SkMatrix and SkPaint paint.

'SKPointMode' determines the drawing method.

Details:

    * If mode is kPoints_PointMode, the shape of point drawn depends on paint
    SkPaint::Cap. If paint is set to SkPaint::kRound_Cap, each point draws a
    circle of diameter SkPaint stroke width. If paint is set to SkPaint::kSquare_Cap
    or SkPaint::kButt_Cap, each point draws a square of width and height
    SkPaint stroke width.

    * If mode is kLines_PointMode, each pair of points draws a line segment.
    One line is drawn for every two points; each point is used once. If count is odd,
    the final point is ignored.

    * If mode is kPolygon_PointMode, each adjacent pair of points draws a line segment.
    count minus one lines are drawn; the first and last point are used once.

    * Each line segment respects paint SkPaint::Cap and SkPaint stroke width.
    SkPaint::Style is ignored, as if were set to SkPaint::kStroke_Style.

    * Always draws each element one at a time; is not affected by
    SkPaint::Join, and unlike drawPath(), does not create a mask from all points
    and lines before drawing.
-}
drawPoints ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    SKPointMode ->
    -- | Points to draw
    VS.Vector Sk_point ->
    SKPaint ->
    m ()
drawPoints (toA SKCanvas -> canvas) pointMode points paint = evalContIO do
    points' <- ContT $ VS.unsafeWith points
    liftIO $
        sk_canvas_draw_points
            (ptr canvas)
            (marshalSKEnum pointMode)
            (fromIntegral (VS.length points))
            points'
            (ptr paint)

{- | Draws point at (x, y) using clip, SkMatrix and SkPaint paint.

The shape of point drawn depends on paint SkPaint::Cap. If paint is set to
SkPaint::kRound_Cap, draw a circle of diameter SkPaint stroke width. If paint
is set to SkPaint::kSquare_Cap or SkPaint::kButt_Cap, draw a square of width
and height SkPaint stroke width. SkPaint::Style is ignored, as if were set to
SkPaint::kStroke_Style.
-}
drawPoint ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | (x, y). left and top edge of circle or square
    V2 Float ->
    -- | Stroke, blend, color, and so on, used to draw
    SKPaint ->
    m ()
drawPoint (toA SKCanvas -> canvas) (V2 x y) paint = evalContIO do
    canvas' <- useObj canvas
    paint' <- useObj paint
    liftIO $
        sk_canvas_draw_point
            canvas'
            (coerce x)
            (coerce y)
            paint'

{- | Draws line segment from (x0, y0) to (x1, y1) using clip, SkMatrix, and
SkPaint paint. In paint: SkPaint stroke width describes the line thickness;
SkPaint::Cap draws the end rounded or square; SkPaint::Style is ignored, as if
were set to SkPaint::kStroke_Style.
-}
drawLine ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | (x0, y0). start of the line segment
    V2 Float ->
    -- | (x1, y1). end of the line segment
    V2 Float ->
    -- | Stroke, blend, color, and so on, used to draw
    SKPaint ->
    m ()
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

{- | Draws SkRect rect using clip, SkMatrix, and SkPaint paint.
In paint: SkPaint::Style determines if rectangle is stroked or filled;
if stroked, SkPaint stroke width describes the line thickness, and
SkPaint::Join draws the corners rounded or square.
-}
drawRect ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | Rectangle to draw
    Rect Float ->
    -- | Stroke, blend, color, and so on, used to draw
    SKPaint ->
    m ()
drawRect (toA SKCanvas -> canvas) rect paint = evalContIO do
    canvas' <- useObj canvas
    rect' <- useStorable (toSKRect rect)
    paint' <- useObj paint
    liftIO $ sk_canvas_draw_rect canvas' rect' paint'

{- | Draws SkRegion region using clip, SkMatrix, and SkPaint paint. In paint:
SkPaint::Style determines if rectangle is stroked or filled; if stroked, SkPaint
stroke width describes the line thickness, and SkPaint::Join draws the corners
rounded or square.
-}
drawRegion ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | Region to draw
    SKRegion ->
    -- | SkPaint stroke or fill, blend, color, and so on, used to draw
    SKPaint ->
    m ()
drawRegion (toA SKCanvas -> canvas) region paint = evalContIO do
    canvas' <- useObj canvas
    region' <- useObj region
    paint' <- useObj paint
    liftIO $ sk_canvas_draw_region canvas' region' paint'

{- | Draws oval oval using clip, SkMatrix, and SkPaint. In paint: SkPaint::Style
determines if oval is stroked or filled; if stroked, SkPaint stroke width
describes the line thickness.
-}
drawOval ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | Bounds of oval
    Rect Float ->
    -- | SkPaint stroke or fill, blend, color, and so on, used to draw
    SKPaint ->
    m ()
drawOval (toA SKCanvas -> canvas) rect paint = evalContIO do
    rect' <- useStorable (toSKRect rect)
    liftIO $ sk_canvas_draw_oval (ptr canvas) rect' (ptr paint)

{- | Draws SkRRect rrect using clip, SkMatrix, and SkPaint paint.
In paint: SkPaint::Style determines if rrect is stroked or filled;
if stroked, SkPaint stroke width describes the line thickness.

rrect may represent a rectangle, circle, oval, uniformly rounded rectangle, or
may have any combination of positive non-square radii for the four corners.
-}
drawRRect ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | rrect. Rect with up to eight corner radii to draw
    SKRRect ->
    -- | SkPaint stroke or fill, blend, color, and so on, used to draw
    SKPaint ->
    m ()
drawRRect (toA SKCanvas -> canvas) rrect paint = evalContIO do
    canvas' <- useObj canvas
    rrect' <- useObj rrect
    paint' <- useObj paint
    liftIO $ sk_canvas_draw_rrect canvas' rrect' paint'

{- | Draws SkRRect outer and inner using clip, SkMatrix, and SkPaint paint.
outer must contain inner or the drawing is undefined. In paint:
SkPaint::Style determines if SkRRect is stroked or filled; if stroked,
SkPaint stroke width describes the line thickness. If stroked and SkRRect
corner has zero length radii, SkPaint::Join can draw corners rounded or
square.

GPU-backed platforms optimize drawing when both outer and inner are concave
and outer contains inner. These platforms may not be able to draw SkPath
built with identical data as fast.
-}
drawDRRect ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | Outer
    SKRRect ->
    -- | Inner
    SKRRect ->
    -- | Stroke, blend, color, and so on, used to draw
    SKPaint ->
    m ()
drawDRRect (toA SKCanvas -> canvas) outer inner paint = evalContIO do
    canvas' <- useObj canvas
    outer' <- useObj outer
    inner' <- useObj inner
    paint' <- useObj paint
    liftIO $ sk_canvas_draw_drrect canvas' outer' inner' paint'

{- | Draws circle at (cx, cy) with radius using clip, SkMatrix, and SkPaint
paint. If radius is zero or less, nothing is drawn. In paint: SkPaint::Style
determines if circle is stroked or filled; if stroked, SkPaint stroke width
describes the line thickness.
-}
drawCircle ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | (cx, cy). Center center
    V2 Float ->
    -- | Circle radius
    Float ->
    -- | Stroke, blend, color, and so on, used to draw
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

{- | Draws arc using clip, SkMatrix, and SkPaint paint.

Arc is part of oval bounded by oval, sweeping from startAngle to startAngle plus
sweepAngle. startAngle and sweepAngle are in degrees.

startAngle of zero places start point at the right middle edge of oval. A
positive sweepAngle places arc end point clockwise from start point; a negative
sweepAngle places arc end point counterclockwise from start point. sweepAngle
may exceed 360 degrees, a full circle. If useCenter is true, draw a wedge that
includes lines from oval center to arc end points. If useCenter is false, draw
arc between end points.

If SkRect oval is empty or sweepAngle is zero, nothing is drawn.
-}
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
    oval' <- useStorable (toSKRect oval)
    paint' <- useObj paint
    liftIO $
        sk_canvas_draw_arc
            canvas'
            oval'
            (coerce startAngle)
            (coerce sweepAngle)
            (fromBool useCenter)
            paint'

{- | Draws SkRRect bounded by SkRect rect, with corner radii (rx, ry) using
clip, SkMatrix, and SkPaint paint.

In paint: SkPaint::Style determines if SkRRect is stroked or filled; if stroked,
SkPaint stroke width describes the line thickness.

If rx or ry are less than zero, they are treated as if they are zero.

If rx plus ry exceeds rect width or rect height, radii are scaled down to fit.

If rx and ry are zero, SkRRect is drawn as SkRect and if stroked is affected by
SkPaint::Join.
-}
drawRoundRect ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | Bounds of 'SKRRect' to draw.
    Rect Float ->
    -- | (rx, ry). Axis lengths on x-axis and y-axis of oval describing rounded corners.
    V2 Float ->
    -- | Stroke, blend, color, and so on, used to draw
    SKPaint ->
    m ()
drawRoundRect (toA SKCanvas -> canvas) rect (V2 rx ry) paint = evalContIO do
    canvas' <- useObj canvas
    rect' <- useStorable (toSKRect rect)
    paint' <- useObj paint
    liftIO $
        sk_canvas_draw_round_rect
            canvas'
            rect'
            (coerce rx)
            (coerce ry)
            paint'

{- | Draws SkPath path using clip, SkMatrix, and SkPaint paint. SkPath contains
an array of path contour, each of which may be open or closed.

In paint: SkPaint::Style determines if SkRRect is stroked or filled: if
filled, SkPath::FillType determines whether path contour describes inside or
outside of fill; if stroked, SkPaint stroke width describes the line
thickness, SkPaint::Cap describes line ends, and SkPaint::Join describes how
corners are drawn.
-}
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

-- | FIXME: Google Skia does not provide a description for this function.
drawImage :: (MonadIO m, IsSKCanvas canvas) => canvas -> SKImage -> V2 Float -> SKSamplingOptions -> SKPaint -> m ()
drawImage (toA SKCanvas -> canvas) im (V2 x y) sampling paint = evalContIO do
    canvas' <- useObj canvas
    im' <- useObj im
    sampling' <- useStorable (marshalSKSamplingOptions sampling)
    paint' <- useObj paint
    liftIO $ sk_canvas_draw_image canvas' im' (coerce x) (coerce y) sampling' paint'

-- | FIXME: Google Skia does not provide a description for this function.
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
    srcRect' <- useStorable (toSKRect srcRect)
    dstRect' <- useStorable (toSKRect dstRect)
    sampling' <- useStorable (marshalSKSamplingOptions sampling)
    paint' <- useObj paint
    liftIO $ sk_canvas_draw_image_rect canvas' im' srcRect' dstRect' sampling' paint'

drawImageLattice ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    SKImage ->
    Sk_lattice ->
    -- | Destination
    Rect Float ->
    SKFilterMode ->
    SKPaint ->
    m ()
drawImageLattice (toA SKCanvas -> canvas) image lattice dst filterMode paint = evalContIO do
    canvas' <- useObj canvas
    image' <- useObj image
    lattice' <- useStorable lattice
    dst' <- useStorable (toSKRect dst)
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
    center' <- useStorable (toSKIRect center)
    dst' <- useStorable (toSKRect dst)
    paint' <- useObj paint
    liftIO $ sk_canvas_draw_image_nine canvas' image' center' dst' (marshalSKEnum filterMode) paint'

{- | Draws text, with origin at (x, y), using clip, SkMatrix, SkFont font, and
SkPaint paint.

When encoding is SkTextEncoding::kUTF8, SkTextEncoding::kUTF16, or
SkTextEncoding::kUTF32, this function uses the default character-to-glyph
mapping from the SkTypeface in font.  It does not perform typeface fallback for
characters not found in the SkTypeface. It does not perform kerning or other
complex shaping; glyphs are positioned based on their default advances.

Text meaning depends on SkTextEncoding.

Text size is affected by SkMatrix and SkFont text size. Default text size is 12
point.

All elements of paint: SkPathEffect, SkMaskFilter, SkShader, SkColorFilter, and
SkImageFilter; apply to text. By default, draws filled black glyphs.
-}
drawSimpleTextEncoding ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | Text data
    BS.ByteString ->
    -- | Text encoding used in text data
    SKTextEncoding ->
    -- | Position
    V2 Float ->
    -- | Typeface, text size and so, used to describe the text
    SKFont ->
    -- | Blend, color, and so on, used to draw
    SKPaint ->
    m ()
drawSimpleTextEncoding (toA SKCanvas -> canvas) textData textEncoding (V2 x y) font paint = evalContIO do
    canvas' <- useObj canvas
    font' <- useObj font
    paint' <- useObj paint

    (text', len) <- ContT $ BS.unsafeUseAsCStringLen textData

    liftIO $
        sk_canvas_draw_simple_text
            canvas'
            (castPtr text')
            (fromIntegral len)
            (marshalSKEnum textEncoding)
            (coerce x)
            (coerce y)
            font'
            paint'

-- | Convenience function. Like 'drawSimpleTextEncoding' but accepts 'T.Text'.
drawSimpleText ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | Text to draw
    T.Text ->
    -- | Position
    V2 Float ->
    SKFont ->
    SKPaint ->
    m ()
drawSimpleText canvas text =
    -- FIXME: T.encodeUtf8 is O(n)...
    -- https://hackage-content.haskell.org/package/text-2.1.2/docs/src/Data.Text.Encoding.html#encodeUtf8
    drawSimpleTextEncoding canvas (T.encodeUtf8 text) SKTextEncoding'UTF8

{- | Draws SkTextBlob blob at (x, y), using clip, SkMatrix, and SkPaint paint.

blob contains glyphs, their positions, and paint attributes specific to text:
SkTypeface, SkPaint text size, SkPaint text scale x, SkPaint text skew x,
SkPaint::Align, SkPaint::Hinting, anti-alias, SkPaint fake bold, SkPaint font
embedded bitmaps, SkPaint full hinting spacing, LCD text, SkPaint linear text,
and SkPaint subpixel text.

SkTextEncoding must be set to SkTextEncoding::kGlyphID.

Elements of paint: anti-alias, SkBlendMode, color including alpha,
SkColorFilter, SkPaint dither, SkMaskFilter, SkPathEffect, SkShader, and
SkPaint::Style; apply to blob. If SkPaint contains SkPaint::kStroke_Style:
SkPaint miter limit, SkPaint::Cap, SkPaint::Join, and SkPaint stroke width;
apply to SkPath created from blob.
-}
drawTextBlob ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | blob. Glyphs, positions, and their paints' text size, typeface, and so on
    SKTextBlob ->
    --  (x, y) offset applied to blob
    V2 Float ->
    -- | paint. Blend, color, stroking, and so on, used to draw
    SKPaint ->
    m ()
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

{- | Draws SkPicture picture, using clip and SkMatrix; transforming picture with
SkMatrix matrix, if provided; and use 'SKPaint' paint alpha, 'SKColorFilter',
'SKImageFilter', and 'SKBlendMode', if provided.

If paint is not 'Nothing', then the picture is always drawn into a temporary
layer before actually landing on the canvas. Note that drawing into a layer can
also change its appearance if there are any non-associative blendModes inside
any of the pictures elements.
-}
drawPicture ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | picture. The recorded drawing commands to play.
    SKPicture ->
    -- | matrix. Optional.
    Maybe (M33 Float) ->
    -- | paint. Optional.
    Maybe SKPaint ->
    m ()
drawPicture (toA SKCanvas -> canvas) picture matrix paint = evalContIO do
    matrix' <- useNullIfNothing useStorable $ fmap toSKMatrix $ matrix
    liftIO $ sk_canvas_draw_picture (ptr canvas) (ptr picture) matrix' (ptrOrNull paint)

{- | Draws SkVertices vertices, a triangle mesh, using clip and SkMatrix. If
paint contains an SkShader and vertices does not contain texCoords, the shader
is mapped using the vertices' positions.

SkBlendMode is ignored if SkVertices does not have colors. Otherwise, it
combines

   * the SkShader if SkPaint contains SkShader

   * or the opaque SkPaint color if SkPaint does not contain SkShader

as the src of the blend and the interpolated vertex colors as the dst.

SkMaskFilter, SkPathEffect, and antialiasing on SkPaint are ignored.
-}
drawVertices ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | vertices. Triangle mesh to draw.
    SKVertices ->
    -- | mode. Combines vertices' colors with SkShader if present or SkPaint opaque color
    -- if not. Ignored if the vertices do not contain color.
    SKBlendMode ->
    -- | paint. Specifies the SkShader, used as SkVertices texture, and
    -- SkColorFilter.
    SKPaint ->
    m ()
drawVertices (toA SKCanvas -> canvas) vertices blendMode paint = liftIO do
    sk_canvas_draw_vertices (ptr canvas) (ptr vertices) (marshalSKEnum blendMode) (ptr paint)

{- | Draws a set of sprites from atlas, using clip, SkMatrix, and optional
SkPaint paint. paint uses anti-alias, alpha, SkColorFilter, SkImageFilter, and
SkBlendMode to draw, if present. For each entry in the array, SkRect tex locates
sprite in atlas, and SkRSXform xform transforms it into destination space.

SkMaskFilter and SkPathEffect on paint are ignored.

xform, tex, and colors (if colors is present) **must contain the same number of
entries**, otherwise a 'BadArgumentError' is raised.

Optional colors are applied for each sprite using SkBlendMode mode, treating
sprite as source and colors as destination.

Optional cullRect is a conservative bounds of all transformed sprites. If
cullRect is outside of clip, canvas can skip drawing.
-}
drawAtlas ::
    forall canvas m.
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | atlas. SkImage containing sprites
    SKImage ->
    -- | xform. SkRSXform mappings for sprites in atlas.
    VS.Vector Sk_rsxform ->
    -- | tex. SkRect locations of sprites in atlas
    VS.Vector Sk_rect ->
    -- | colors. Optiona. One per sprite, blended with sprite using SkBlendMode.
    Maybe (VS.Vector SKColor) ->
    -- | mode. SkBlendMode combining colors and sprites
    SKBlendMode ->
    -- | sampling. SkSamplingOptions used when sampling from the atlas image
    SKSamplingOptions ->
    -- | bounds. Optional. Bounds of transformed sprites for efficient clipping
    Maybe (Rect Float) ->
    -- | paint. Optional. SkColorFilter, SkImageFilter, SkBlendMode, and so on.
    Maybe SKPaint ->
    m ()
drawAtlas (toA SKCanvas -> canvas) atlas xform tex colors mode sampling cullRect paint = evalContIO do
    when (VS.length xform /= VS.length tex) do
        liftIO . throwIO . BadArgumentError $
            "length of xform (="
                <> show (VS.length xform)
                <> ") must must match the length of tex (="
                <> show (VS.length tex)
                <> ")"

    -- NOTE: VS.length tex is also ok
    let count = VS.length xform

    whenJust colors \colors -> do
        when (VS.length colors /= count) do
            liftIO . throwIO . BadArgumentError $
                "length of colors (="
                    <> show (VS.length colors)
                    <> ") must match the length of xform and tex (="
                    <> show count
                    <> ")"

    xform' <- useStorableVector xform
    tex' <- useStorableVector tex
    colors' <- useNullIfNothing useStorableVector colors

    sampling' <- useStorable (marshalSKSamplingOptions sampling)
    cullRect' <- useNullIfNothing useStorable $ fmap toSKRect $ cullRect

    liftIO $
        sk_canvas_draw_atlas
            (ptr canvas)
            (ptr atlas)
            xform'
            tex'
            (coercePtr colors')
            (fromIntegral count)
            (marshalSKEnum mode)
            sampling'
            cullRect'
            (ptrOrNull paint)

drawPatch ::
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
drawPatch (toA SKCanvas -> canvas) cubics colors texCoords blendMode paint = evalContIO do
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

{- | Draws SkDrawable drawable using clip and SkMatrix, concatenated with
optional matrix.

If SkCanvas has an asynchronous implementation, as is the case when it is
recording into SkPicture, then drawable will be referenced, so that
SkDrawable::draw() can be called when the operation is finalized. To force
immediate drawing, call SkDrawable::draw() instead.
-}
drawDrawable ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | drawable. Custom struct encapsulating drawing commands
    SKDrawable ->
    -- | matrix. Optional. Transformation applied to drawing
    Maybe (M33 Float) ->
    m ()
drawDrawable (toA SKCanvas -> canvas) drawable matrix = evalContIO do
    matrix' <- useNullIfNothing useStorable $ fmap toSKMatrix $ matrix
    liftIO $ sk_canvas_draw_drawable (ptr canvas) (ptr drawable) matrix'

{- | Associates SkRect on SkCanvas with an annotation; a key-value pair, where
the key is a null-terminated UTF-8 string, and optional value is stored as
SkData.

Only some canvas implementations, such as recording to SkPicture, or drawing to
document PDF, use annotations.
-}
drawAnnotation ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    -- | rect. Extent of canvas to annotate
    Rect Float ->
    -- | key. String used for lookup
    T.Text ->
    -- | value. Data holding value stored in annotation.
    SKData ->
    m ()
drawAnnotation (toA SKCanvas -> canvas) rect key value = evalContIO do
    rect' <- useStorable (toSKRect rect)
    key' <- useTextAsUtf8CString key
    liftIO $ sk_canvas_draw_annotation (ptr canvas) rect' key' (ptr value)

drawUrlAnnotation ::
    (MonadIO m, IsSKCanvas canvas) =>
    canvas ->
    Rect Float ->
    -- | Value
    SKData ->
    m ()
drawUrlAnnotation (toA SKCanvas -> canvas) rect value = evalContIO do
    canvas' <- useObj canvas
    rect' <- useStorable (toSKRect rect)
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
    rect' <- useStorable (toSKRect rect)
    value' <- useObj value
    liftIO $ sk_canvas_draw_link_destination_annotation canvas' rect' value'

-- * Queries

{- | Returns true if clip is empty; that is, nothing will draw.

May do work when called; it should not be called more often than needed.
However, once called, subsequent calls perform no work until clip changes.
-}
isClipEmpty :: (MonadIO m, IsSKCanvas canvas) => canvas -> m Bool
isClipEmpty (toA SKCanvas -> canvas) = evalContIO do
    canvas' <- useObj canvas
    liftIO $ fmap toBool $ sk_canvas_is_clip_empty canvas'

{- | Returns true if clip is 'Rect' and not empty.

Returns false if the clip is empty, or if it is not 'Rect'.
-}
isClipRect :: (MonadIO m, IsSKCanvas canvas) => canvas -> m Bool
isClipRect (toA SKCanvas -> canvas) = evalContIO do
    canvas' <- useObj canvas
    liftIO $ fmap toBool $ sk_canvas_is_clip_rect canvas'

{- | Returns the current transform from local coordinates to the 'device', which
for most purposes means pixels.
-}
getLocalToDevice :: (MonadIO m, IsSKCanvas canvas) => canvas -> m (M44 Float)
getLocalToDevice (toA SKCanvas -> canvas) = evalContIO do
    canvas' <- useObj canvas
    matrix' <- useAlloca
    -- NOTE: This is named @SkM44 getLocalToDevice() const;@ in Google Skia. We
    -- will follow that instead of 'sk_canvas_get_matrix'.
    liftIO $ sk_canvas_get_matrix canvas' matrix'
    peekWith fromSKMatrix44 matrix'

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