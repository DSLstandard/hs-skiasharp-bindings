module Skia.SKPathMeasure where

import Data.Bits
import Linear
import Skia.Internal.Prelude

-- | Destroy a 'SKPathMeasure'.
destroy :: (MonadIO m) => SKPathMeasure -> m ()
destroy measure = evalContIO do
    measure' <- useObj measure
    liftIO $ sk_pathmeasure_destroy measure'

{- | Create an uninitialized 'SKPathMeasure'. You may then use 'setPath' to
configure it. Consider using 'createWithPath" to do all in one step.

You must manually free the returned 'SKPathMeasure' with 'destroy' when done
with it.
-}
create :: (MonadIO m) => m SKPathMeasure
create = liftIO do
    measure' <- sk_pathmeasure_new
    toObject measure'

{- | Create and initialize the pathmeasure with the specified path. The parts of
the path that are needed are copied, so the client is free to modify/delete the
path after this call.

resScale controls the precision of the measure. values > 1 increase the
precision (and possibly slow down the computation).

You must manually free the returned 'SKPathMeasure' with 'destroy' when done
with it.
-}
createWithPath ::
    (MonadIO m) =>
    SKPath ->
    -- | forceClosed
    Bool ->
    -- | resScale controls the precision of the measure. values > 1 increase the
    -- precision (and possibly slow down the computation).
    --
    -- For reference, Google Skia's puts @1@ here as the default.
    Float ->
    m SKPathMeasure
createWithPath path forceClosed resScale = evalContIO do
    path' <- useObj path
    measure' <- liftIO $ sk_pathmeasure_new_with_path path' (fromBool forceClosed) (coerce resScale)
    toObject measure'

{- | Return the total length of the current contour, or 0 if no path is
associated.
-}
getLength :: (MonadIO m) => SKPathMeasure -> m Float
getLength measure = evalContIO do
    measure' <- useObj measure
    liftIO $ fmap coerce $ sk_pathmeasure_get_length measure'

{- | Pins distance to 0 <= distance <= getLength(), and then computes the
corresponding position and tangent. Returns 'Nothing' if there is no path, or a
zero-length path was specified, in which case position and tangent are
unchanged.
-}
getPosTan ::
    (MonadIO m) =>
    SKPathMeasure ->
    -- | \"distance\"
    Float ->
    -- | Returns the position and tangent vector, 'Nothing' if there is no path.
    m (Maybe (V2 Float, V2 Float))
getPosTan measure distance = evalContIO do
    measure' <- useObj measure

    position' <- useAlloca
    tangent' <- useAlloca

    ok <- liftIO $ fmap toBool $ sk_pathmeasure_get_pos_tan measure' (coerce distance) position' tangent'
    if ok
        then do
            position <- peekWith fromSKVector position'
            tangent <- peekWith fromSKVector tangent'
            pure $ Just (position, tangent)
        else do
            pure Nothing

-- | Used by 'getMatrix'.
data MatrixFlags
    = MatrixFlags'GetPos
    | MatrixFlags'GetTan
    | MatrixFlags'GetPosAndTan
    deriving (Show, Eq, Ord)

{- | Pins distance to 0 <= distance <= getLength(), and then computes the
corresponding matrix (by calling getPosTan). Returns false if there is no path,
or a zero-length path was specified, in which case 'Nothing' is returned.
-}
getMatrix ::
    (MonadIO m) =>
    SKPathMeasure ->
    -- | \"distance\"
    Float ->
    MatrixFlags ->
    m (Maybe (M33 Float))
getMatrix measure distance flags = evalContIO do
    measure' <- useObj measure
    matrix' <- useAlloca

    let
        bitField = case flags of
            MatrixFlags'GetPos -> GET_POSITION_SK_PATHMEASURE_MATRIXFLAGS
            MatrixFlags'GetTan -> GET_TANGENT_SK_PATHMEASURE_MATRIXFLAGS
            MatrixFlags'GetPosAndTan -> GET_POSITION_SK_PATHMEASURE_MATRIXFLAGS .|. GET_TANGENT_SK_PATHMEASURE_MATRIXFLAGS

    ok <- liftIO $ fmap toBool $ sk_pathmeasure_get_matrix measure' (coerce distance) matrix' bitField
    if ok
        then do
            Just <$> peekWith fromSKMatrix matrix'
        else do
            pure Nothing

{- | Given a start and stop distance, return in dst the intervening segment(s).

If the segment is zero-length, return false, else return true. startD and stopD
are pinned to legal values (0..getLength()). If startD > stopD then return false
(and leave dst untouched).

Begin the segment with a moveTo if startWithMoveTo is true
-}
getSegment ::
    (MonadIO m) =>
    SKPathMeasure ->
    -- | (startD, stopD)
    (Float, Float) ->
    -- | \"dst\"
    SKPath ->
    -- | \"startWithMoveTo\"
    Bool ->
    m Bool
getSegment measure (startD, stopD) dst startWithMoveTo = evalContIO do
    measure' <- useObj measure
    dst' <- useObj dst
    liftIO $ fmap toBool $ sk_pathmeasure_get_segment measure' (coerce startD) (coerce stopD) dst' (fromBool startWithMoveTo)

{- | Return true if the current contour is closed()

You may want to see 'nextContour'.
-}
isClosed :: (MonadIO m) => SKPathMeasure -> m Bool
isClosed measure = evalContIO do
    measure' <- useObj measure
    liftIO $ fmap toBool $ sk_pathmeasure_is_closed measure'

{- | Move to the next contour in the path. Return true if one exists, or false if
we're done with the path.
-}
nextContour :: (MonadIO m) => SKPathMeasure -> m Bool
nextContour measure = evalContIO do
    measure' <- useObj measure
    liftIO $ fmap toBool $ sk_pathmeasure_next_contour measure'
