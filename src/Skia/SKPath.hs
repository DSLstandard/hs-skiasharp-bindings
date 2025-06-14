module Skia.SKPath where

import Data.ByteString qualified as BS
import Linear
import Skia.Internal.Prelude
import Skia.SKRoundRect qualified as SKRoundRect
import Skia.SKString qualified as SKString
import Skia.Types.Rect qualified as Rect

-- | Free a 'SKPath'.
destroy :: (MonadIO m) => SKPath -> m ()
destroy path = evalContIO do
    path' <- useObj path
    liftIO $ sk_path_delete path'

{- | Constructs an empty SkPath. By default, SkPath has no verbs, no SkPoint,
and no weights. FillType is set to kWinding.

You must free the returned 'SKPath' with 'destroy' when done with it.
-}
create :: (MonadIO m) => m SKPath
create = evalContIO do
    path' <- liftIO $ sk_path_new
    toObject path'

{- | Clone a 'SKPath'.

You must free the returned 'SKPath' with 'destroy' when done with it.
-}
clone :: (MonadIO m) => SKPath -> m SKPath
clone path = evalContIO do
    path' <- useObj path
    newPath' <- liftIO $ sk_path_clone path'
    toObject newPath'

-- | Adds beginning of contour at a position.
moveTo :: (MonadIO m) => SKPath -> V2 Float -> m ()
moveTo path p = evalContIO do
    path' <- useObj path
    liftIO $ sk_path_move_to path' & applyV2 (coerce p)

rMoveTo :: (MonadIO m) => SKPath -> V2 Float -> m ()
rMoveTo path dp = evalContIO do
    path' <- useObj path
    liftIO $ sk_path_rmove_to path' & applyV2 (coerce dp)

lineTo :: (MonadIO m) => SKPath -> V2 Float -> m ()
lineTo path p = evalContIO do
    path' <- useObj path
    liftIO $ sk_path_line_to path' & applyV2 (coerce p)

rLineTo :: (MonadIO m) => SKPath -> V2 Float -> m ()
rLineTo path dp = evalContIO do
    path' <- useObj path
    liftIO $ sk_path_rline_to path' & applyV2 (coerce dp)

quadTo ::
    (MonadIO m) =>
    SKPath ->
    -- | Control point 1
    V2 Float ->
    -- | End point
    V2 Float ->
    m ()
quadTo path c1 c2 = evalContIO do
    path' <- useObj path
    liftIO $ sk_path_quad_to path' & applyV2 (coerce c1) & applyV2 (coerce c2)

rQuadTo ::
    (MonadIO m) =>
    SKPath ->
    -- | Control point 1
    V2 Float ->
    -- | End point
    V2 Float ->
    m ()
rQuadTo path c1 c2 = evalContIO do
    path' <- useObj path
    liftIO $ sk_path_rquad_to path' & applyV2 (coerce c1) & applyV2 (coerce c2)

cubicTo ::
    (MonadIO m) =>
    SKPath ->
    -- | Control point 1
    V2 Float ->
    -- | Control point 2
    V2 Float ->
    -- | End point
    V2 Float ->
    m ()
cubicTo path c1 c2 c3 = evalContIO do
    path' <- useObj path
    liftIO $ sk_path_cubic_to path' & applyV2 (coerce c1) & applyV2 (coerce c2) & applyV2 (coerce c3)

rCubicTo ::
    (MonadIO m) =>
    SKPath ->
    -- | Control point 1
    V2 Float ->
    -- | Control point 2
    V2 Float ->
    -- | End point
    V2 Float ->
    m ()
rCubicTo path c1 c2 c3 = evalContIO do
    path' <- useObj path
    liftIO $ sk_path_rcubic_to path' & applyV2 (coerce c1) & applyV2 (coerce c2) & applyV2 (coerce c3)

conicTo ::
    (MonadIO m) =>
    SKPath ->
    -- | Control point 1
    V2 Float ->
    -- | End point
    V2 Float ->
    -- | Weight of the added conic
    Float ->
    m ()
conicTo path c1 c2 weight = evalContIO do
    path' <- useObj path
    liftIO $ sk_path_conic_to path' & applyV2 (coerce c1) & applyV2 (coerce c2) & apply (coerce weight)

rConicTo ::
    (MonadIO m) =>
    SKPath ->
    -- | Control point 1
    V2 Float ->
    -- | End point
    V2 Float ->
    -- | Weight of the added conic
    Float ->
    m ()
rConicTo path c1 c2 weight = evalContIO do
    path' <- useObj path
    liftIO $ sk_path_rconic_to path' & applyV2 (coerce c1) & applyV2 (coerce c2) & apply (coerce weight)

arcTo ::
    (MonadIO m) =>
    SKPath ->
    -- | Radii on axes before x-axis rotation
    V2 Float ->
    -- | X-axis rotation in degrees
    Degrees ->
    -- | Chooses smaller or larger arc
    SKPathArcSize ->
    -- | Chooses closewise or counterclockwise
    SKPathDirection ->
    -- | End of arc
    V2 Float ->
    m ()
arcTo path radii xAxisRotate largeArc sweep xy = evalContIO do
    path' <- useObj path
    liftIO $
        sk_path_arc_to path'
            & applyV2 (coerce radii)
            & apply (coerce xAxisRotate)
            & apply (marshalSKEnum largeArc)
            & apply (marshalSKEnum sweep)
            & applyV2 (coerce xy)

rArcTo ::
    (MonadIO m) =>
    SKPath ->
    -- | Radii on axes before x-axis rotation
    V2 Float ->
    -- | X-axis rotation in degrees
    Degrees ->
    -- | Chooses smaller or larger arc
    SKPathArcSize ->
    -- | Chooses closewise or counterclockwise
    SKPathDirection ->
    -- | End of arc
    V2 Float ->
    m ()
rArcTo path radii xAxisRotate largeArc sweep dxdy = evalContIO do
    path' <- useObj path
    liftIO $
        sk_path_rarc_to path'
            & applyV2 (coerce radii)
            & apply (coerce xAxisRotate)
            & apply (marshalSKEnum largeArc)
            & apply (marshalSKEnum sweep)
            & applyV2 (coerce dxdy)

arcToWithOval ::
    (MonadIO m) =>
    SKPath ->
    -- | Oval
    Rect Float ->
    -- | Start angle
    Float ->
    -- | Sweep angle
    Float ->
    -- | Force move to?
    Bool ->
    m ()
arcToWithOval path oval startAngle sweepAngle forceMoveTo = evalContIO do
    path' <- useObj path
    oval' <- useStorable $ toSKRect oval
    liftIO $ sk_path_arc_to_with_oval path' oval' (coerce startAngle) (coerce sweepAngle) (fromBool forceMoveTo)

arcToWithPoints ::
    (MonadIO m) =>
    SKPath ->
    -- | Common point to pair of tangents
    V2 Float ->
    -- | End of second tangent
    V2 Float ->
    -- | Radius: Distance from arc to circle center
    Float ->
    m ()
arcToWithPoints path c1 c2 radius = evalContIO do
    path' <- useObj path
    liftIO $
        sk_path_arc_to_with_points path'
            & applyV2 (coerce c1)
            & applyV2 (coerce c2)
            & apply (coerce radius)

close :: (MonadIO m) => SKPath -> m ()
close path = evalContIO do
    path' <- useObj path
    liftIO $ sk_path_close path'

data RectPoint
    = RectPoint'TopLeft
    | RectPoint'RightTop
    | RectPoint'BottomRight
    | RectPoint'BottomLeft
    deriving (Show, Eq, Ord, Enum, Bounded)

{- | PRIVATE FUNCTION. Maps 'RectPoint' to their corresponding \"startIndex\".
Used by functions like 'sk_path_add_rrect_start'.

See src/core/SkPathMakers.h's SkPath_RectPointIterator.
-}
privRectPointToStartIndex :: RectPoint -> Word32
privRectPointToStartIndex = \case
    RectPoint'TopLeft -> 0
    RectPoint'RightTop -> 1
    RectPoint'BottomRight -> 2
    RectPoint'BottomLeft -> 3

addRect :: (MonadIO m) => (MonadIO m) => SKPath -> Rect Float -> SKPathDirection -> m ()
addRect path rect dir = evalContIO do
    path' <- useObj path
    rect' <- useStorable $ toSKRect rect
    liftIO $ sk_path_add_rect path' rect' (marshalSKEnum dir)

addRectWithStart :: (MonadIO m) => (MonadIO m) => SKPath -> Rect Float -> SKPathDirection -> RectPoint -> m ()
addRectWithStart path rect dir startPoint = evalContIO do
    path' <- useObj path
    rect' <- useStorable $ toSKRect rect
    liftIO $ sk_path_add_rect_start path' rect' (marshalSKEnum dir) (privRectPointToStartIndex startPoint)

addRRect :: (MonadIO m) => (MonadIO m) => SKPath -> SKRoundRect -> SKPathDirection -> m ()
addRRect path rrect dir = evalContIO do
    path' <- useObj path
    rrect' <- useObj rrect
    liftIO $ sk_path_add_rrect path' rrect' (marshalSKEnum dir)

addRRectWithStart :: (MonadIO m) => (MonadIO m) => SKPath -> SKRoundRect -> SKPathDirection -> RectPoint -> m ()
addRRectWithStart path rrect dir startPoint = evalContIO do
    path' <- useObj path
    rrect' <- useObj rrect
    liftIO $ sk_path_add_rrect_start path' rrect' (marshalSKEnum dir) (privRectPointToStartIndex startPoint)

addRoundedRect ::
    (MonadIO m) =>
    SKPath ->
    Rect Float ->
    -- | Radii
    V2 Float ->
    SKPathDirection ->
    m ()
addRoundedRect path rect radii dir = evalContIO do
    path' <- useObj path
    rect' <- useStorable $ toSKRect rect
    liftIO $
        sk_path_add_rounded_rect path'
            & apply rect'
            & applyV2 (coerce radii)
            & apply (marshalSKEnum dir)

addOval :: (MonadIO m) => SKPath -> Rect Float -> SKPathDirection -> m ()
addOval path rect dir = evalContIO do
    path' <- useObj path
    rect' <- useStorable $ toSKRect rect
    liftIO $ sk_path_add_oval path' rect' (marshalSKEnum dir)

addCircle ::
    (MonadIO m) =>
    SKPath ->
    -- | Circle center
    V2 Float ->
    -- | Radius
    Float ->
    SKPathDirection ->
    m ()
addCircle path center radius dir = evalContIO do
    path' <- useObj path
    liftIO $
        sk_path_add_circle path'
            & applyV2 (coerce center)
            & apply (coerce radius)
            & apply (marshalSKEnum dir)

addArc ::
    (MonadIO m) =>
    SKPath ->
    Rect Float ->
    -- | Start angle in degrees
    Degrees ->
    -- | Sweep angle in degrees
    Degrees ->
    m ()
addArc path oval startAngle sweepAngle = evalContIO do
    path' <- useObj path
    oval' <- useStorable $ toSKRect oval
    liftIO $ sk_path_add_arc path' oval' (coerce startAngle) (coerce sweepAngle)

addPolyRaw ::
    (MonadIO m) =>
    SKPath ->
    -- | Points array
    Ptr Sk_point ->
    -- | Number of elements in points array
    Int ->
    -- | If true, add line connecting contour end and start.
    Bool ->
    m ()
addPolyRaw path pts count close = evalContIO do
    path' <- useObj path
    liftIO $ sk_path_add_poly path' pts (fromIntegral count) (fromBool close)

-- | Like 'addPolyRaw' but takes in a list of (V2 Float) as points.
addPolyByList ::
    (MonadIO m) =>
    SKPath ->
    [V2 Float] ->
    -- | If true, add line connecting contour end and start.
    Bool ->
    m ()
addPolyByList path pts close = evalContIO do
    -- FIXME: Optimize? fmapping is bad.
    (pts', numPts) <- ContT $ withArrayLen' $ fmap toSKPoint $ pts
    addPolyRaw path pts' numPts close

{- | Returns minimum and maximum axes values of SkPoint array. Returns 'Nothing'
if the path contains no points.

Returned bounds width and height may be larger or smaller than area affected
when SkPath is drawn.
-}
getBounds :: (MonadIO m) => SKPath -> m (Maybe (Rect Float))
getBounds path = evalContIO do
    path' <- useObj path
    bounds' <- useAlloca

    liftIO $ sk_path_get_bounds path' bounds'

    bounds <- liftIO $ fromSKRect <$> peek bounds'
    if Rect.isEmpty bounds
        then pure Nothing
        else pure (Just bounds)

{- | Returns minimum and maximum axes values of the lines and curves in SkPath.
Returns (0, 0, 0, 0) if SkPath contains no points. Returned bounds width and
height may be larger or smaller than area affected when SkPath is drawn.

Includes SkPoint associated with kMove_Verb that define empty contours.

Behaves identically to getBounds() when SkPath contains only lines. If SkPath
contains curves, computed bounds includes the maximum extent of the quad, conic,
or cubic; is slower than getBounds(); and unlike getBounds(), does not cache the
result.
-}
computeTightBounds :: (MonadIO m) => SKPath -> m (Maybe (Rect Float))
computeTightBounds path = evalContIO do
    path' <- useObj path
    bounds' <- useAlloca

    liftIO $ sk_path_compute_tight_bounds path' bounds'

    bounds <- liftIO $ fromSKRect <$> peek bounds'
    if Rect.isEmpty bounds
        then pure Nothing
        else pure (Just bounds)

getFillType :: (MonadIO m) => SKPath -> m SKPathFillType
getFillType path = evalContIO do
    path' <- useObj path
    filltype <- liftIO $ sk_path_get_filltype path'
    unmarshalSKEnumOrDie filltype

setFillType :: (MonadIO m) => SKPath -> SKPathFillType -> m ()
setFillType path filltype = evalContIO do
    path' <- useObj path
    liftIO $ sk_path_set_filltype path' (marshalSKEnum filltype)

transform :: (MonadIO m) => SKPath -> M33 Float -> m ()
transform path matrix = evalContIO do
    path' <- useObj path
    matrix' <- useStorable $ toSKMatrix matrix
    liftIO $ sk_path_transform path' matrix'

transformToDest ::
    (MonadIO m) =>
    SKPath ->
    M33 Float ->
    -- | Destination path.
    SKPath ->
    m ()
transformToDest path matrix dstPath = evalContIO do
    path' <- useObj path
    dstPath' <- useObj dstPath
    matrix' <- useStorable $ toSKMatrix matrix
    liftIO $ sk_path_transform_to_dest path' matrix' dstPath'

addPath ::
    (MonadIO m) =>
    SKPath ->
    -- | Other
    SKPath ->
    SKPathAddMode ->
    m ()
addPath path other addMode = evalContIO do
    path' <- useObj path
    other' <- useObj other
    liftIO $ sk_path_add_path path' other' (marshalSKEnum addMode)

addPathReversed ::
    (MonadIO m) =>
    SKPath ->
    -- | Other
    SKPath ->
    m ()
addPathReversed path other = evalContIO do
    path' <- useObj path
    other' <- useObj other
    liftIO $ sk_path_add_path_reverse path' other'

addPathWithOffset ::
    (MonadIO m) =>
    SKPath ->
    -- | Other
    SKPath ->
    -- | Offset
    V2 Float ->
    SKPathAddMode ->
    m ()
addPathWithOffset path other offset addMode = evalContIO do
    path' <- useObj path
    other' <- useObj other
    liftIO $
        sk_path_add_path_offset path' other'
            & applyV2 (coerce offset)
            & apply (marshalSKEnum addMode)

addPathWithTransform ::
    (MonadIO m) =>
    SKPath ->
    -- | Other
    SKPath ->
    -- | Transform
    M33 Float ->
    SKPathAddMode ->
    m ()
addPathWithTransform path other matrix addMode = evalContIO do
    path' <- useObj path
    other' <- useObj other
    matrix' <- useStorable $ toSKMatrix matrix
    liftIO $ sk_path_add_path_matrix path' other' matrix' (marshalSKEnum addMode)

reset :: (MonadIO m) => SKPath -> m ()
reset path = evalContIO do
    path' <- useObj path
    liftIO $ sk_path_reset path'

rewind :: (MonadIO m) => SKPath -> m ()
rewind path = evalContIO do
    path' <- useObj path
    liftIO $ sk_path_rewind path'

countPoints :: (MonadIO m) => SKPath -> m Int
countPoints path = evalContIO do
    path' <- useObj path
    liftIO $ fromIntegral <$> sk_path_count_points path'

{- | Returns SkPoint at index in SkPoint array. Valid range for index is 0 to
countPoints() - 1. Returns (0, 0) if index is out of range.
-}
getPoint ::
    (MonadIO m) =>
    SKPath ->
    -- | Index
    Int32 ->
    m (V2 Float)
getPoint path index = evalContIO do
    path' <- useObj path
    point' <- useAlloca
    liftIO $ sk_path_get_point path' (coerce index) point'
    liftIO $ fromSKPoint <$> peek point'

{- | Returns number of points in SkPath. Up to max points are copied.  points
may be nullptr; then, max must be zero.  If max is greater than number of
points, excess points storage is unaltered.

Returns SkPoint array length
-}
getPointsToDest ::
    (MonadIO m) =>
    SKPath ->
    -- | Destination array.
    Ptr Sk_point ->
    -- | Maximum count to copy. Must be greater than or equal to zero.
    Int32 ->
    m Int32
getPointsToDest path dstPoints maxCount = evalContIO do
    path' <- useObj path
    liftIO $ coerce <$> sk_path_get_points path' dstPoints (coerce maxCount)

countVerbs :: (MonadIO m) => SKPath -> m Int
countVerbs path = evalContIO do
    path' <- useObj path
    liftIO $ fromIntegral <$> sk_path_count_verbs path'

{- | Returns true if the point (x, y) is contained by SkPath, taking into
account FillType.
-}
containsPoint :: (MonadIO m) => SKPath -> V2 Float -> m Bool
containsPoint path p = evalContIO do
    path' <- useObj path
    liftIO $ toBool <$> (sk_path_contains path' & applyV2 (coerce p))

parseSVGToDestRaw ::
    (MonadIO m) =>
    -- | Destination path
    SKPath ->
    -- | Null-terminated C string
    CString ->
    -- | Returns false if failed.
    m Bool
parseSVGToDestRaw dstPath str' = evalContIO do
    dstPath' <- useObj dstPath
    liftIO $ toBool <$> sk_path_parse_svg_string dstPath' str'

{- | Like 'parseSVGToDestRaw' but takes in a ByteString. However, it does an
O(n) copy of the ByteString to add a null-terminator at the end.
-}
parseSVGToDest ::
    (MonadIO m) =>
    -- | Destination path
    SKPath ->
    BS.ByteString ->
    m Bool
parseSVGToDest dstPath str = liftIO do
    BS.useAsCString str \str' -> do
        parseSVGToDestRaw dstPath str'

renderSvgString :: (MonadIO m) => SKPath -> m SKString
renderSvgString path = do
    str <- SKString.createEmpty

    evalContIO do
        path' <- useObj path
        str' <- useObj str
        liftIO $ sk_path_to_svg_string path' str'

    pure str

-- | Returns the last point on the path. Returns 'Nothing' if the path is empty.
getLastPoint :: (MonadIO m) => SKPath -> m (Maybe (V2 Float))
getLastPoint path = evalContIO do
    path' <- useObj path
    point' <- useAlloca

    exists <- liftIO $ toBool <$> sk_path_get_last_point path' point'
    if exists
        then do
            point <- liftIO $ fromSKPoint <$> peek point'
            pure (Just point)
        else do
            pure Nothing

isConvex :: (MonadIO m) => SKPath -> m Bool
isConvex path = evalContIO do
    path' <- useObj path
    liftIO $ toBool <$> sk_path_is_convex path'

asRect ::
    (MonadIO m) =>
    SKPath ->
    -- | Returns (rect bounds, 'true' if the 'SKPath' is closed, rect path
    -- direction). Returns 'Nothing' if 'SKPath' is not a rectangle.
    m (Maybe (Rect Float, Bool, SKPathDirection))
asRect path = evalContIO do
    path' <- useObj path

    bounds' <- useAlloca
    isClosed' <- useAlloca
    direction' <- useAlloca

    isrect <- liftIO $ fmap toBool $ sk_path_is_rect path' bounds' isClosed' direction'

    if isrect
        then do
            bounds <- peekWith fromSKRect bounds'
            isClosed <- peekWith toBool isClosed'
            direction <- unmarshalSKEnumOrDie =<< peekWith id direction'
            pure $ Just (bounds, isClosed, direction)
        else do
            pure Nothing

asOval :: (MonadIO m) => SKPath -> m (Maybe (Rect Float))
asOval path = evalContIO do
    path' <- useObj path
    bounds' <- useAlloca

    exists <- liftIO $ fmap toBool $ sk_path_is_oval path' bounds'
    if exists
        then do
            bounds <- liftIO $ fromSKRect <$> peek bounds'
            pure (Just bounds)
        else do
            pure Nothing

asRRect :: (MonadIO m) => SKPath -> m (Maybe SKRoundRect)
asRRect path = evalContIO do
    rrect <- SKRoundRect.create

    path' <- useObj path
    rrect' <- useObj rrect

    exists <- liftIO $ toBool <$> sk_path_is_rrect path' rrect'
    if exists
        then do
            pure (Just rrect)
        else do
            disposeObject rrect
            pure Nothing

{- | Like 'isLine', but returns 'Nothing' if false; returns the (start, end)
points of the line if true.
-}
asLine :: (MonadIO m) => SKPath -> m (Maybe (V2 Float, V2 Float))
asLine path = evalContIO do
    path' <- useObj path

    -- NOTE: 'sk_path_is_line' takes 'sk_point_t line[2]' instead of two
    -- separate 'sk_point_t's, which it makes the code here slightly ugly.

    points' <- ContT $ allocaArray 2

    exists <- liftIO $ toBool <$> sk_path_is_line path' points'
    if exists
        then do
            start <- liftIO $ fromSKPoint <$> peekElemOff points' 0
            end <- liftIO $ fromSKPoint <$> peekElemOff points' 1
            pure (Just (start, end))
        else do
            pure Nothing

getSegmentMasks :: (MonadIO m) => SKPath -> m Word32
getSegmentMasks path = evalContIO do
    path' <- useObj path
    liftIO $ sk_path_get_segment_masks path'

{- | Returns true if operation was able to produce a result; otherwise, result
is unmodified.
-}
opToDest ::
    (MonadIO m) =>
    -- | Path 1
    SKPath ->
    -- | Path 2
    SKPath ->
    -- | Operation mode
    SKPathOp ->
    -- | Destination path
    SKPath ->
    m Bool
opToDest path1 path2 opMode dstPath = evalContIO do
    path1' <- useObj path1
    path2' <- useObj path2
    dstPath' <- useObj dstPath
    liftIO $ toBool <$> sk_pathop_op path1' path2' (marshalSKEnum opMode) dstPath'

{- | Like 'opToDest' but creates the path for you and returns it. Returns
'Nothing' if failed.
-}
op ::
    (MonadIO m) =>
    -- | Path 1
    SKPath ->
    -- | Path 2
    SKPath ->
    -- | Operation mode
    SKPathOp ->
    m (Maybe SKPath)
op path1 path2 opMode = do
    dstPath <- create
    success <- opToDest path1 path2 opMode dstPath
    if success
        then do
            pure $ Just dstPath
        else do
            disposeObject dstPath
            pure Nothing

{- | Set this path to a set of non-overlapping contours that describe the same
area as the original path. The curve order is reduced where possible so that
cubics may be turned into quadratics, and quadratics maybe turned into lines.

Returns true if operation was able to produce a result; otherwise, result is
unmodified.
-}
simplifyToDest ::
    (MonadIO m) =>
    SKPath ->
    -- | Destination path
    SKPath ->
    m Bool
simplifyToDest path dstPath = evalContIO do
    path' <- useObj path
    dstPath' <- useObj dstPath
    liftIO $ toBool <$> sk_pathop_simplify path' dstPath'

{- | Like 'simplifyPathToDest' but creates the path for you and returns it. Returns
'Nothing' if failed.
-}
simplify :: (MonadIO m) => SKPath -> m (Maybe SKPath)
simplify path = do
    dstPath <- create
    success <- simplifyToDest path dstPath
    if success
        then do
            pure $ Just dstPath
        else do
            disposeObject dstPath
            pure Nothing

{- | Computes the rectangle of the tight bounds of the path. Returns 'Nothing' if
the bounds could not be computed.
-}
tightBounds :: (MonadIO m) => SKPath -> m (Maybe (Rect Float))
tightBounds path = evalContIO do
    path' <- useObj path
    bounds' <- useAlloca
    success <- liftIO $ toBool <$> sk_pathop_tight_bounds path' bounds'
    if success
        then do
            bounds <- liftIO $ fromSKRect <$> peek bounds'
            pure $ Just bounds
        else do
            pure Nothing

-- TODO: I don't understand what 'toWindingToDest' does.

{- | Set the result with fill type winding to area equivalent to path. Returns
 true if successful. Does not detect if path contains contours which contain
 self-crossings or cross other contours; in these cases, may return true even
 though result does not fill same area as path.

 Returns true if operation was able to produce a result; otherwise, result is
 unmodified. The result may be the input.
-}
toWindingToDest :: (MonadIO m) => SKPath -> SKPath -> m Bool
toWindingToDest path result = evalContIO do
    path' <- useObj path
    result' <- useObj result
    liftIO $ toBool <$> sk_pathop_as_winding path' result'

toWinding :: (MonadIO m) => SKPath -> m (Maybe SKPath)
toWinding path = do
    result <- create
    success <- toWindingToDest path result
    if success
        then do
            pure $ Just result
        else do
            disposeObject result
            pure Nothing
