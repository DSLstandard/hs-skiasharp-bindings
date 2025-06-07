module Skia.SKPathOpBuilder where

import Skia.Internal.Prelude
import Skia.SKPath qualified as SKPath

destroy :: (MonadIO m) => SKPathOpBuilder -> m ()
destroy builder = evalContIO do
    builder' <- useObj builder
    liftIO $ sk_opbuilder_destroy builder'

create :: (MonadIO m) => m SKPathOpBuilder
create = liftIO do
    builder' <- sk_opbuilder_new
    toObject builder'

add :: (MonadIO m) => SKPathOpBuilder -> SKPath -> SKPathOp -> m ()
add builder path op = evalContIO do
    builder' <- useObj builder
    path' <- useObj path
    liftIO $ sk_opbuilder_add builder' path' (marshalSKEnum op)

{- | Writes the path built by the op builder to the destination path. Returns
true if successful. Returns false if failed.
-}
resolveToDest ::
    (MonadIO m) =>
    SKPathOpBuilder ->
    -- | Destination path
    SKPath ->
    m Bool
resolveToDest builder dstPath = evalContIO do
    builder' <- useObj builder
    dstPath' <- useObj dstPath
    liftIO $ toBool <$> sk_opbuilder_resolve builder' dstPath'

{- | Like 'resolveToDest' but creates the path for you and returns it. Returns
'Nothing' if failed.
-}
resolve :: (MonadIO m) => SKPathOpBuilder -> m (Maybe SKPath)
resolve builder = do
    path <- SKPath.create
    success <- resolveToDest builder path
    if success
        then do
            pure (Just path)
        else do
            disposeObject path
            pure Nothing
