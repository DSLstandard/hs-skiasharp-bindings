module Skia.SKDynamicMemoryWStream where

import Skia.Internal.Prelude

-- | Creates an empty 'SKDynamicMemoryWStream'.
create :: Acquire SKDynamicMemoryWStream
create = mkSKObjectAcquire sk_dynamicmemorywstream_new sk_dynamicmemorywstream_destroy

{- | Copies 'Skia.SKWStream.getBytesWritten' bytes of data from the current
stream to a memory location.
-}
copyTo ::
    (MonadIO m) =>
    SKDynamicMemoryWStream ->
    -- | Destination buffer. The buffer size must be at least 'Skia.SKWStream.getBytesWritten'
    Ptr Word8 ->
    m ()
copyTo stream buffer = liftIO do
    sk_dynamicmemorywstream_copy_to (ptr stream) (castPtr buffer)

{- | Copies 'Skia.SKWStream.getBytesWritten' bytes of data from the current
stream to the destination stream.
-}
writeToStream ::
    (MonadIO m, IsSKWStream dest) =>
    SKDynamicMemoryWStream ->
    -- | Destination stream.
    dest ->
    m Bool
writeToStream stream (toA SKWStream -> dest) = liftIO do
    toBool <$> sk_dynamicmemorywstream_write_to_stream (ptr stream) (ptr dest)

{- | Returns an 'SKData' instance of the data in the current stream, and then
resets the current stream.
-}
detachAsData :: SKDynamicMemoryWStream -> Acquire SKData
detachAsData stream =
    mkSKObjectAcquire
        (sk_dynamicmemorywstream_detach_as_data (ptr stream))
        sk_data_unref

{- | Returns a read-only stream with the current data, and then resets the
current stream.
-}
detachAsStream :: SKDynamicMemoryWStream -> Acquire SKStreamAsset
detachAsStream stream =
    mkSKObjectAcquire
        (sk_dynamicmemorywstream_detach_as_stream (ptr stream))
        sk_stream_asset_destroy
