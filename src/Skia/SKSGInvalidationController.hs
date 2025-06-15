module Skia.SKSGInvalidationController where

import Linear
import Skia.Internal.Prelude
import Skia.Types.Rect qualified as Rect

-- | Destroys an 'SKSGInvalidationController'.
destroy :: (MonadIO m) => SKSGInvalidationController -> m ()
destroy ic = evalContIO do
    ic' <- useObj ic
    liftIO $ sksg_invalidation_controller_delete ic'

-- | Creates an 'SKSGInvalidationController'.
create :: (MonadIO m) => m (Owned SKSGInvalidationController)
create = liftIO do
    ic' <- sksg_invalidation_controller_new
    toObject ic'

-- | Adds a rectangle.
invalidate ::
    (MonadIO m) =>
    SKSGInvalidationController ->
    -- | Rect to invalidate
    Rect Float ->
    -- | Transformation. Consider using 'Linear.identity'.
    --
    -- Note that the transformation is naive - the transformation is applied to
    -- all corners of the input rectangle to yield another **axis-aligned**
    -- rectangle, which is then added.
    M33 Float ->
    m ()
invalidate ic rect transform = evalContIO do
    ic' <- useObj ic
    rect' <- useStorable $ toSKRect rect
    transform' <- useStorable $ toSKMatrix transform
    liftIO $ sksg_invalidation_controller_inval ic' rect' transform'

{- | Returns the current maximum bounding box. O(1) operation.

Returns 'Nothing' if the maximum bounding box is empty, which is the case
when 'invalidate' is never called.
-}
getBounds :: (MonadIO m) => SKSGInvalidationController -> m (Maybe (Rect Float))
getBounds ic = evalContIO do
    ic' <- useObj ic
    bounds' <- useAlloca
    liftIO $ sksg_invalidation_controller_get_bounds ic' bounds'
    bounds <- peekWith fromSKRect bounds'
    pure $ if Rect.isEmpty bounds then Nothing else Just bounds

{- | Resets the invalidation controller.

This operation simply clears out the list of rectangles stored in 'SKSGInvalidationController'.
-}
reset :: (MonadIO m) => SKSGInvalidationController -> m ()
reset ic = evalContIO do
    ic' <- useObj ic
    liftIO $ sksg_invalidation_controller_reset ic'
