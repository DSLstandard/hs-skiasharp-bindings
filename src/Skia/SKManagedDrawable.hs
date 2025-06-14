module Skia.SKManagedDrawable where

import Skia.Internal.Prelude

data DrawableParams = DrawableParams
    { draw :: SKCanvas -> IO ()
    , getBounds :: IO (Rect Float)
    , getApproximateBytesUsed :: IO Int
    , makePictureSnapshot :: IO SKPicture
    , destroy :: IO ()
    }

initializeProcs :: (MonadIO m) => m ()
initializeProcs = liftIO do
    fDraw <- mkFunPtr'Sk_manageddrawable_draw_proc $ \_ ctx canvas' -> do
        params <- interpretCtx ctx
        canvas <- toObject canvas'
        params.draw canvas

    fGetBounds <- mkFunPtr'Sk_manageddrawable_getBounds_proc $ \_ ctx rect' -> do
        params <- interpretCtx ctx
        bounds <- params.getBounds
        poke rect' (toSKRect bounds)

    fApproximateBytesUsed <- mkFunPtr'Sk_manageddrawable_approximateBytesUsed_proc $ \_ ctx -> do
        params <- interpretCtx ctx
        fromIntegral <$> params.getApproximateBytesUsed

    fMakePictureSnapshot <- mkFunPtr'Sk_manageddrawable_makePictureSnapshot_proc $ \_ ctx -> do
        params <- interpretCtx ctx
        picture <- params.makePictureSnapshot
        disownObject picture

    fDestroy <- mkFunPtr'Sk_manageddrawable_destroy_proc $ \_ ctx -> do
        params <- interpretCtx ctx
        params.destroy

    sk_manageddrawable_set_procs Sk_manageddrawable_procs{..}
  where
    interpretCtx :: Ptr () -> IO DrawableParams
    interpretCtx ptr = deRefStablePtr (castPtrToStablePtr ptr)

createFromParams :: (MonadIO m) => DrawableParams -> m SKManagedDrawable
createFromParams params = liftIO do
    rec -- We modify 'params' here to hook in a 'freeStablePtr' to delete
        -- `params'` when the managed drawable is destroyed.
        let paramsUpdated =
                params
                    { destroy = do
                        params.destroy
                        freeStablePtr params'
                    }
        params' <- newStablePtr paramsUpdated

    drawable' <- sk_manageddrawable_new (castStablePtrToPtr params')
    toObjectFin sk_manageddrawable_unref drawable'
