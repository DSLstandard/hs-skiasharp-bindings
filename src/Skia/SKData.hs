module Skia.SKData where

import Skia.Internal.Prelude

createEmpty :: (MonadIO m) => m SKData
createEmpty = liftIO do
    dat' <- sk_data_new_empty
    toObjectFin sk_data_unref dat'

createCopy ::
    (MonadIO m) =>
    -- | Buffer
    Ptr Word8 ->
    -- | Buffer size
    Int ->
    m SKData
createCopy ptr count = liftIO do
    dat' <- sk_data_new_with_copy (castPtr ptr) (fromIntegral count)
    toObjectFin sk_data_unref dat'

createWithSize ::
    (MonadIO m) =>
    -- | Number of bytes
    Int ->
    m SKData
createWithSize size = liftIO do
    dat' <- sk_data_new_uninitialized (fromIntegral size)
    toObjectFin sk_data_unref dat'

createSubset ::
    (MonadIO m) =>
    SKData ->
    -- | Offset
    Int ->
    -- | Length
    Int ->
    m SKData
createSubset dat offset len = evalContIO do
    dat' <- useObj dat
    subset' <- liftIO $ sk_data_new_subset dat' (fromIntegral offset) (fromIntegral len)
    toObjectFin sk_data_unref subset'

createFromFile :: (MonadIO m) => FilePath -> m SKData
createFromFile path = liftIO do
    withCString path \path' -> do
        dat' <- sk_data_new_from_file path'
        toObjectFin sk_data_unref dat'

createFromStream ::
    (IsSKStream stream, MonadIO m) =>
    stream ->
    -- | Length
    Int ->
    m SKData
createFromStream (toA SKStream -> stream) len = evalContIO do
    stream' <- useObj stream
    dat' <- liftIO $ sk_data_new_from_stream stream' (fromIntegral len)
    toObjectFin sk_data_unref dat'

-- | Returns the number of bytes stored.
getSize :: (MonadIO m) => SKData -> m Int
getSize dat = evalContIO do
    dat' <- useObj dat
    liftIO $ fromIntegral <$> sk_data_get_size dat'

-- | Returns a read-only pointer to the data.
getData :: (MonadIO m) => SKData -> m (Ptr Word8)
getData dat = evalContIO do
    dat' <- useObj dat
    liftIO $ sk_data_get_bytes dat'

-- TODO: What is context?
createWithRelease ::
    (MonadIO m) =>
    -- | Buffer
    Ptr Word8 ->
    -- | Buffer size
    Int ->
    -- | Release callback
    IO () ->
    m SKData
createWithRelease buffer size releaseCallback = liftIO do
    -- We use ctx to pass the FunPtr handle to the release callback so it can
    -- free itself with freeHaskellFunPtr.

    let
        dataReleaseProc :: Sk_data_release_proc
        dataReleaseProc _buffer ctx = do
            releaseCallback
            freeHaskellFunPtr (castPtrToFunPtr ctx)

    releaseCallback' <- mkFunPtr'Sk_data_release_proc dataReleaseProc
    dat' <-
        sk_data_new_with_proc
            (castPtr buffer)
            (fromIntegral size)
            releaseCallback'
            (castFunPtrToPtr releaseCallback') -- 'ctx' param
    toObjectFin sk_data_unref dat'
