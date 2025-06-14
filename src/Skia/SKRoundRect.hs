module Skia.SKRoundRect where

import Data.Traversable
import Linear
import Skia.Internal.Prelude

delete :: (MonadIO m) => SKRoundRect -> m ()
delete rrect = evalContIO do
    rrect' <- useObj rrect
    liftIO $ sk_rrect_delete rrect'

data Radii a = Radii
    { upperLeft :: a
    , upperRight :: a
    , lowerRight :: a
    , lowerLeft :: a
    }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

{- | A Radii with each field set to the corresponding 'SKRoundRectCorner' enum
value.

This is used for doing traversal tricks.
-}
radiiCorners :: Radii SKRoundRectCorner
radiiCorners =
    Radii
        { upperLeft = SKRoundRectCorner'UpperLeft
        , upperRight = SKRoundRectCorner'UpperRight
        , lowerRight = SKRoundRectCorner'LowerRight
        , lowerLeft = SKRoundRectCorner'LowerLeft
        }

-- | Creates an empty SKRoundRect.
create :: (MonadIO m) => m SKRoundRect
create = liftIO do
    rrect' <- sk_rrect_new
    toObject rrect'

clone :: (MonadIO m) => SKRoundRect -> m SKRoundRect
clone rrect = evalContIO do
    rrect' <- useObj rrect
    newRRect' <- liftIO $ sk_rrect_new_copy rrect'
    toObject newRRect'

getType :: (MonadIO m) => SKRoundRect -> m SKRoundRectType
getType rrect = evalContIO do
    rrect' <- useObj rrect
    t <- liftIO $ sk_rrect_get_type rrect'
    unmarshalSKEnumOrDie t

getRect :: (MonadIO m) => SKRoundRect -> m (Rect Float)
getRect rrect = evalContIO do
    rrect' <- useObj rrect
    rect' <- useAlloca
    liftIO $ sk_rrect_get_rect rrect' rect'
    liftIO $ fromSKRect <$> peek rect'

getRadii :: (MonadIO m) => SKRoundRect -> SKRoundRectCorner -> m (V2 Float)
getRadii rrect corner = evalContIO do
    rrect' <- useObj rrect
    radii' <- useAlloca
    liftIO $ sk_rrect_get_radii rrect' (marshalSKEnum corner) radii'
    liftIO $ fromSKPoint <$> peek radii'

getAllRadii :: (MonadIO m) => SKRoundRect -> m (Radii (V2 Float))
getAllRadii rrect = evalContIO do
    for radiiCorners \corner -> do
        getRadii rrect corner

getWidth :: (MonadIO m) => SKRoundRect -> m Float
getWidth rrect = evalContIO do
    rrect' <- useObj rrect
    liftIO $ coerce <$> sk_rrect_get_width rrect'

getHeight :: (MonadIO m) => SKRoundRect -> m Float
getHeight rrect = evalContIO do
    rrect' <- useObj rrect
    liftIO $ coerce <$> sk_rrect_get_height rrect'

setEmpty :: (MonadIO m) => SKRoundRect -> m ()
setEmpty rrect = evalContIO do
    rrect' <- useObj rrect
    liftIO $ sk_rrect_set_empty rrect'

setRect :: (MonadIO m) => SKRoundRect -> Rect Float -> m ()
setRect rrect rect = evalContIO do
    rrect' <- useObj rrect
    rect' <- useStorable $ toSKRect rect
    liftIO $ sk_rrect_set_rect rrect' rect'

setOval :: (MonadIO m) => SKRoundRect -> Rect Float -> m ()
setOval rrect oval = evalContIO do
    rrect' <- useObj rrect
    oval' <- useStorable $ toSKRect oval
    liftIO $ sk_rrect_set_oval rrect' oval'

setRectXY ::
    (MonadIO m) =>
    SKRoundRect ->
    Rect Float ->
    -- | X and Y radius
    V2 Float ->
    m ()
setRectXY rrect rect (V2 rx ry) = evalContIO do
    rrect' <- useObj rrect
    rect' <- useStorable $ toSKRect rect
    liftIO $ sk_rrect_set_rect_xy rrect' rect' (coerce rx) (coerce ry)

setRectRadii ::
    (MonadIO m) =>
    SKRoundRect ->
    Rect Float ->
    Radii (V2 Float) ->
    m ()
setRectRadii rrect rect radii = evalContIO do
    rrect' <- useObj rrect
    rect' <- useStorable $ toSKRect rect

    let array = toSKPoint <$> [radii.upperLeft, radii.upperRight, radii.lowerRight, radii.lowerLeft]
    radii' <- ContT $ withArray array
    liftIO $ sk_rrect_set_rect_radii rrect' rect' radii'

setNinePatch ::
    (MonadIO m) =>
    SKRoundRect ->
    -- | bounds of rounded rectangle
    Rect Float ->
    -- | left-top and left-bottom x-axis radius
    Float ->
    -- | left-top and right-top y-axis radius
    Float ->
    -- | right-top and right-bottom x-axis radius
    Float ->
    -- | left-bottom and right-bottom y-axis radius
    Float ->
    m ()
setNinePatch rrect rect leftRad topRad rightRad bottomRad = evalContIO do
    rrect' <- useObj rrect
    rect' <- useStorable $ toSKRect rect
    liftIO $ sk_rrect_set_nine_patch rrect' rect' (coerce leftRad) (coerce topRad) (coerce rightRad) (coerce bottomRad)

inset :: (MonadIO m) => SKRoundRect -> V2 Float -> m ()
inset rrect (V2 dx dy) = evalContIO do
    rrect' <- useObj rrect
    liftIO $ sk_rrect_inset rrect' (coerce dx) (coerce dy)

outset :: (MonadIO m) => SKRoundRect -> V2 Float -> m ()
outset rrect (V2 dx dy) = evalContIO do
    rrect' <- useObj rrect
    liftIO $ sk_rrect_outset rrect' (coerce dx) (coerce dy)

offset :: (MonadIO m) => SKRoundRect -> V2 Float -> m ()
offset rrect (V2 dx dy) = evalContIO do
    rrect' <- useObj rrect
    liftIO $ sk_rrect_offset rrect' (coerce dx) (coerce dy)

containsRect :: (MonadIO m) => SKRoundRect -> Rect Float -> m Bool
containsRect rrect rect = evalContIO do
    rrect' <- useObj rrect
    rect' <- useStorable $ toSKRect rect
    liftIO $ toBool <$> sk_rrect_contains rrect' rect'

isValid :: (MonadIO m) => SKRoundRect -> m Bool
isValid rrect = evalContIO do
    rrect' <- useObj rrect
    liftIO $ toBool <$> sk_rrect_is_valid rrect'

-- | Returns true when successful. Returns false when failed.
transformToDest ::
    (MonadIO m) =>
    SKRoundRect ->
    M33 Float ->
    -- | Destination round rect
    SKRoundRect ->
    m Bool
transformToDest rrect matrix dstRRect = evalContIO do
    rrect' <- useObj rrect
    matrix' <- useStorable $ toSKMatrix matrix
    dstRRect' <- useObj dstRRect
    liftIO $ toBool <$> sk_rrect_transform rrect' matrix' dstRRect'

{- | Like 'transformToDest' but creates the SKRoundRect for you and returns it.
Returns 'Nothing' if failed.
-}
transform ::
    (MonadIO m) =>
    SKRoundRect ->
    M33 Float ->
    m (Maybe SKRoundRect)
transform rrect matrix = evalContIO do
    dstRRect <- create
    success <- transformToDest rrect matrix dstRRect
    if success
        then do
            pure (Just dstRRect)
        else do
            disposeObject dstRRect
            pure Nothing
