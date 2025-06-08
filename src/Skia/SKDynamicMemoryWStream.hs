module Skia.SKDynamicMemoryWStream where

import Skia.Internal.Prelude

destroy :: (MonadIO m) => SKDynamicMemoryWStream -> m ()
destroy stream = evalContIO do
    stream' <- useObj stream
    liftIO $ sk_dynamicmemorywstream_destroy stream'

create :: (MonadIO m) => m (Maybe SKDynamicMemoryWStream)
create = liftIO do
    stream' <- liftIO sk_dynamicmemorywstream_new
    toObjectMaybe stream'

copyTo ::
    (MonadIO m) =>
    SKDynamicMemoryWStream ->
    -- | Destination buffer.
    --
    -- WARNING: The caller is responsible for making sure that the destination
    -- buffer is big enough.
    Ptr Word8 ->
    m ()
copyTo stream buffer = evalContIO do
    stream' <- useObj stream
    liftIO $ sk_dynamicmemorywstream_copy_to stream' (castPtr buffer)

writeToStream ::
    (MonadIO m, IsSKWStream dest) =>
    SKDynamicMemoryWStream ->
    -- | Destination buffer.
    --
    -- WARNING: The caller is responsible for making sure that the destination
    -- buffer is big enough.
    dest ->
    m Bool
writeToStream stream (toA SKWStream -> dest) = evalContIO do
    stream' <- useObj stream
    dest' <- useObj dest
    liftIO $ toBool <$> sk_dynamicmemorywstream_write_to_stream stream' dest'

detachAsStream :: (MonadIO m) => SKDynamicMemoryWStream -> m SKStreamAsset
detachAsStream stream = evalContIO do
    stream' <- useObj stream
    asset' <- liftIO $ sk_dynamicmemorywstream_detach_as_stream stream'
    toObject asset'

detachAsData :: (MonadIO m) => SKDynamicMemoryWStream -> m SKData
detachAsData stream = evalContIO do
    stream' <- useObj stream
    dat' <- liftIO $ sk_dynamicmemorywstream_detach_as_data stream'
    toObjectFin sk_data_unref dat'
