module Skia.SKData where

import Control.Exception
import Data.ByteString qualified as BS
import Skia.Internal.Prelude
import Skia.SKRefCnt qualified as SKRefCnt

-- * Creating 'SKData'

{- | Returns a new empty dataref (or a reference to a shared empty dataref).

The returned 'SKData' is unreferenced when 'Acquire' releases.
-}
createEmpty :: Acquire SKData
createEmpty = mkSKObjectAcquire sk_data_new_empty sk_data_unref

{- | O(n). Create a new dataref by copying the input 'BS.ByteString'.

The returned 'SKData' is unreferenced when 'Acquire' releases.
-}
createCopy :: BS.ByteString -> Acquire SKData
createCopy bytestring =
    mkSKObjectAcquire
        {-
            TODO: Possible O(1) implementation?

            -- The StablePtr keeps the ByteString's alive, which in turn keeps the
            -- 'ForeignPtr' data pointer alive, until 'createWithRelease' is done.
            bsStablePtr :: StablePtr BS.ByteString <- newStablePtr bs
            let onRelease = freeStablePtr bsStablePtr
            let (dataptr, len) = BS.toForeignPtr0 bs
            createWithRelease (unsafeForeignPtrToPtr dataptr) (fromIntegral len) onRelease
        -}
        ( evalContIO do
            (ptr, count) <- ContT $ BS.useAsCStringLen bytestring
            liftIO $ sk_data_new_with_copy (castPtr ptr) (fromIntegral count)
        )
        sk_data_unref

{- | Create a new data with uninitialized contents.

The returned 'SKData' is unreferenced when 'Acquire' releases.
-}
createUninitialized ::
    -- | Number of bytes to allocate. The content will be uninitialized.
    Int ->
    Acquire SKData
createUninitialized size =
    mkSKObjectAcquire
        (sk_data_new_uninitialized (fromIntegral size))
        sk_data_unref

{- | Create a new dataref using a subset of the data in the specified src
dataref.

The returned 'SKData' is unreferenced when 'Acquire' releases.
-}
createSubset ::
    SKData ->
    -- | Byte offset
    Int ->
    -- | Byte length
    Int ->
    Acquire SKData
createSubset dat offset len =
    mkSKObjectAcquire
        (sk_data_new_subset (ptr dat) (fromIntegral offset) (fromIntegral len))
        sk_data_unref

{- | Create a new dataref by reading from a file given its 'FilePath'.

Raises 'SkiaError' on failure.

The returned 'SKData' is unreferenced when 'Acquire' releases.
-}
createFromFile :: FilePath -> Acquire SKData
createFromFile path =
    mkSKObjectAcquire
        ( evalContIO do
            path' <- ContT $ withCString path
            dat' <- liftIO $ sk_data_new_from_file path'
            when (dat' == nullPtr) do
                liftIO $ throwIO $ SkiaError $ "Cannot create SKData from file path: " <> show path
            pure dat'
        )
        sk_data_unref

{- | Attempt to read size bytes into a 'SkData'.

If the read succeeds, return the data, else an 'SkiaError' is raised. Either way
the stream's cursor may have been changed as a result of calling read().

The returned 'SKData' is unreferenced when 'Acquire' releases.
-}
createFromStream ::
    (IsSKStream stream) =>
    stream ->
    -- | Number of bytes to read from stream
    Int ->
    Acquire SKData
createFromStream (toA SKStream -> stream) len =
    mkSKObjectAcquire
        (sk_data_new_from_stream (ptr stream) (fromIntegral len))
        sk_data_unref

{- | Create a new dataref, taking the ptr as is, and using the release callback
to free it.

The returned 'SKData' is unreferenced when 'Acquire' releases.
-}
createWithRelease ::
    -- | ptr. Pointer to buffer
    Ptr Word8 ->
    -- | Buffer byte size
    Int ->
    -- | Release callback
    IO () ->
    Acquire SKData
createWithRelease buffer size releaseCallback = do
    releaseFun' <-
        mkAutoReleasingFunPtr
            mkFunPtr'Sk_data_release_proc
            (\_buffer _ctx -> releaseCallback)

    mkSKObjectAcquire
        ( sk_data_new_with_proc
            (castPtr buffer)
            (fromIntegral size)
            releaseFun'
            nullPtr -- 'ctx' param. Set to nullptr as dummy
        )
        sk_data_unref

-- * Misc utilities

-- | Returns the number of bytes stored.
getSize :: (MonadIO m) => SKData -> m Int
getSize dat = evalContIO do
    dat' <- useObj dat
    liftIO $ fromIntegral <$> sk_data_get_size dat'

{- | Exposes the **read-only** pointer to the data.

You must not modify the content under the pointer.

NOTE: Because the underlying 'SKData' should outlive the returned pointer, this
'Acquire' does 'holdReferenceCount' on the input 'SKData' to keep it alive.
-}
acquireData :: SKData -> Acquire (Ptr Word8)
acquireData dat = do
    SKRefCnt.holdReferenceCountNV dat
    liftIO $ sk_data_get_bytes (ptr dat)

{- | O(n). Convenience function. Builds a 'BS.ByteString' by copying from the
input 'SKData'.
-}
getByteString :: (MonadIO m) => SKData -> m BS.ByteString
getByteString dat = liftIO do
    Data.Acquire.with (acquireData dat) \ptr -> do
        sz <- getSize dat
        BS.packCStringLen (castPtr ptr, fromIntegral sz)
