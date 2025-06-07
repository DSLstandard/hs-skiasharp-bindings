module Skia.SKManagedWStream where

import Skia.Internal.Prelude
import System.IO

data WStreamParams = WStreamParams
    { write :: Ptr Word8 -> Int -> IO Bool
    , flush :: IO ()
    , getBytesWritten :: IO Int
    , close :: IO ()
    -- ^ Destroy this stream.
    }

initializeProcs :: (MonadIO m) => m ()
initializeProcs = liftIO do
    fWrite <- mkFunPtr'Sk_managedwstream_write_proc $ \_ ctx buffer size -> do
        params <- interpretCtx ctx
        fromBool <$> params.write (castPtr buffer) (fromIntegral size)

    fFlush <- mkFunPtr'Sk_managedwstream_flush_proc $ \_ ctx -> do
        params <- interpretCtx ctx
        params.flush

    fBytesWritten <- mkFunPtr'Sk_managedwstream_bytesWritten_proc $ \_ ctx -> do
        params <- interpretCtx ctx
        fromIntegral <$> params.getBytesWritten

    fDestroy <- mkFunPtr'Sk_managedwstream_destroy_proc $ \_ ctx -> do
        params <- interpretCtx ctx
        params.close

    sk_managedwstream_set_procs Sk_managedwstream_procs{..}
  where
    interpretCtx :: Ptr () -> IO WStreamParams
    interpretCtx ptr = deRefStablePtr (castPtrToStablePtr ptr)

destroy :: (MonadIO m) => SKManagedWStream -> m ()
destroy stream = evalContIO do
    stream' <- useObj stream
    liftIO $ sk_managedwstream_destroy stream'

createFromParams :: (MonadIO m) => WStreamParams -> m SKManagedWStream
createFromParams params = liftIO do
    rec -- We modify 'params' here to hook in a 'freeStablePtr' to delete
        -- `params'` when the managed stream is destroyed.
        let paramsUpdated =
                params
                    { close = do
                        params.close
                        freeStablePtr params'
                    }
        params' <- newStablePtr paramsUpdated

    stream' <- sk_managedwstream_new (castStablePtrToPtr params')
    toObject stream'

createFromHandle :: (MonadIO m) => Handle -> m SKManagedWStream
createFromHandle handle = liftIO do
    createFromParams
        WStreamParams
            { write = \buffer size -> do
                hPutBuf handle buffer size
                pure True
            , flush = hFlush handle
            , getBytesWritten = fromIntegral <$> hTell handle
            , close = hClose handle
            }
