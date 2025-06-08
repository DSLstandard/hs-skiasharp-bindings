module Skia.SKData where

import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BS
import Foreign.ForeignPtr.Unsafe
import Foreign.StablePtr
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
    m (Ref SKData)
createCopy ptr count = liftIO do
    dat' <- sk_data_new_with_copy (castPtr ptr) (fromIntegral count)
    toObjectFin sk_data_unref dat'

createWithSize ::
    (MonadIO m) =>
    -- | Number of bytes
    Int ->
    m (Ref SKData)
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
    m (Ref SKData)
createSubset dat offset len = evalContIO do
    dat' <- useObj dat
    subset' <- liftIO $ sk_data_new_subset dat' (fromIntegral offset) (fromIntegral len)
    toObjectFin sk_data_unref subset'

createFromFile :: (MonadIO m) => FilePath -> m (Ref SKData)
createFromFile path = liftIO do
    withCString path \path' -> do
        dat' <- sk_data_new_from_file path'
        toObjectFin sk_data_unref dat'

createFromStream ::
    (IsSKStream stream, MonadIO m) =>
    stream ->
    -- | Length
    Int ->
    m (Ref SKData)
createFromStream (toA SKStream -> stream) len = evalContIO do
    stream' <- useObj stream
    dat' <- liftIO $ sk_data_new_from_stream stream' (fromIntegral len)
    toObjectFin sk_data_unref dat'

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

{- | O(1). Creates an 'SKData' that points to the same data as the input
'BS.ByteString'. Note that this operation is *fairly* cheap, so feel free to
convert 'ByteString's to 'SKData's whenever you have to.

Low-level details: This function uses 'createWithRelease' to build a 'StablePtr'
that points to the input 'BS.ByteString', which is freed with 'freeStablePtr'
when the release function is called. There is no concern about use-after-free on
the input 'BS.ByteString'.
-}
createFromByteString :: (MonadIO m) => BS.ByteString -> m (Ref SKData)
createFromByteString bs = liftIO do
    -- The StablePtr keeps the ByteString's alive, which in turn keeps the
    -- 'ForeignPtr' data pointer alive, until 'createWithRelease' is done.
    bsStablePtr :: StablePtr BS.ByteString <- newStablePtr bs
    let onRelease = freeStablePtr bsStablePtr
    let (dataptr, len) = BS.toForeignPtr0 bs
    createWithRelease (unsafeForeignPtrToPtr dataptr) (fromIntegral len) onRelease

-- | Returns the number of bytes stored.
getSize :: (MonadIO m) => SKData -> m Int
getSize dat = evalContIO do
    dat' <- useObj dat
    liftIO $ fromIntegral <$> sk_data_get_size dat'

{- | Exposes the read-only pointer to the data.

You can not modify the content in the pointer.
-}
withData :: (MonadIO m) => SKData -> (Ptr Word8 -> IO r) -> m r
withData dat f = evalContIO do
    dat' <- useObj dat
    bytes' <- liftIO $ sk_data_get_bytes dat'
    liftIO $ f bytes'

-- | O(n). Builds a 'ByteString' by copying from a 'SKData'.
getAsByteString :: (MonadIO m) => SKData -> m BS.ByteString
getAsByteString dat = evalContIO do
    withData dat \ptr -> do
        sz <- getSize dat
        BS.packCStringLen (castPtr ptr, fromIntegral sz)
