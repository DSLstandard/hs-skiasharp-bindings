module Skia.SKManagedStream where

import Skia.Internal.Prelude
import System.IO

data StreamParams = StreamParams
    { read :: Ptr Word8 -> Int -> IO Int
    -- ^ (Destination buffer pointer -> Max read size -> IO (Number of bytes
    -- read))
    --
    -- Generates data and writes to the destination buffer when the stream is
    -- requested to be read.
    , peek :: Ptr Word8 -> Int -> IO Int
    -- ^ (Destination buffer pointer -> Max peek size -> IO (Number of bytes
    -- peeked))
    --
    -- Generates data and writes to the destination buffer when the stream is
    -- requested to be peeked.
    , isAtEnd :: IO Bool
    -- ^ Returns 'True' if the stream is at the end.
    , hasPosition :: IO Bool
    -- ^ Returns true if the stream can report its byte position.
    , getPosition :: IO Int
    -- ^ Returns the byte position of the stream.
    --
    -- If this cannot be done, returns 0.
    , hasLength :: IO Bool
    -- ^ Returns true if the stream can report its byte length.
    , getLength :: IO Int
    -- ^ Returns the byte length of the stream.
    --
    -- If this cannot be done, returns 0.
    , rewind :: IO Bool
    -- ^ Rewinds to the beginning of the stream. Returns true if the stream is
    -- known to be at the beginning after this call returns.
    , seek :: Int -> IO Bool
    -- ^ ((Seek byte position) -> IO ())
    --
    -- Seeks to an absolute position in the stream. If this cannot be done,
    -- returns false. If an attempt is made to seek past the end of the stream,
    -- the position will be set to the end of the stream.
    , move :: Int -> IO Bool
    -- ^ ((Move byte offset) -> IO ())
    --
    -- Seeks to an relative offset in the stream. If this cannot be done,
    -- returns false. If an attempt is made to move to a position outside the
    -- stream, the position will be set to the closest point within the stream
    -- (beginning or end).
    , duplicate :: IO (Maybe SKManagedStream)
    -- ^ Duplicates this stream. If this cannot be done, returns 'Nothing'. The
    -- returned stream will be positioned at the beginning of its data.
    , fork :: IO (Maybe SKManagedStream)
    -- ^ Duplicates this stream. If this cannot be done, returns 'Nothing'. The
    -- returned stream will be positioned the same as this stream.
    , close :: IO ()
    -- ^ Destroy this stream.
    }

initializeProcs :: (MonadIO m) => m ()
initializeProcs = liftIO do
    fRead <- mkFunPtr'Sk_managedstream_read_proc $ \_ ctx buffer maxReadSize -> do
        params <- interpretCtx ctx
        readSize <- params.read (castPtr buffer) (fromIntegral maxReadSize)
        pure $ fromIntegral readSize

    fPeek <- mkFunPtr'Sk_managedstream_peek_proc $ \_ ctx buffer maxReadSize -> do
        params <- interpretCtx ctx
        readSize <- params.peek (castPtr buffer) (fromIntegral maxReadSize)
        pure $ fromIntegral readSize

    fIsAtEnd <- mkFunPtr'Sk_managedstream_isAtEnd_proc $ \_ ctx -> do
        params <- interpretCtx ctx
        fromBool <$> params.isAtEnd

    fHasPosition <- mkFunPtr'Sk_managedstream_hasPosition_proc $ \_ ctx -> do
        params <- interpretCtx ctx
        fromBool <$> params.hasPosition

    fGetPosition <- mkFunPtr'Sk_managedstream_getPosition_proc $ \_ ctx -> do
        params <- interpretCtx ctx
        fromIntegral <$> params.getPosition

    fHasLength <- mkFunPtr'Sk_managedstream_hasLength_proc $ \_ ctx -> do
        params <- interpretCtx ctx
        fromBool <$> params.hasLength

    fGetLength <- mkFunPtr'Sk_managedstream_getLength_proc $ \_ ctx -> do
        params <- interpretCtx ctx
        fromIntegral <$> params.getLength

    fRewind <- mkFunPtr'Sk_managedstream_rewind_proc $ \_ ctx -> do
        params <- interpretCtx ctx
        fromBool <$> params.rewind

    fSeek <- mkFunPtr'Sk_managedstream_seek_proc $ \_ ctx position -> do
        params <- interpretCtx ctx
        fromBool <$> params.seek (fromIntegral position)

    fMove <- mkFunPtr'Sk_managedstream_move_proc $ \_ ctx offset -> do
        params <- interpretCtx ctx
        fromBool <$> params.move (fromIntegral offset)

    fDuplicate <- mkFunPtr'Sk_managedstream_duplicate_proc $ \_ ctx -> do
        params <- interpretCtx ctx
        params.duplicate >>= \case
            Nothing -> do
                -- Skia's doc says:
                --
                -- Duplicates this stream. If this cannot be done, returns NULL.
                -- The returned stream will be positioned at the beginning of
                -- its data.
                pure nullPtr
            Just newStream -> do
                disownObject newStream

    fFork <- mkFunPtr'Sk_managedstream_fork_proc $ \_ ctx -> do
        params <- interpretCtx ctx
        params.fork >>= \case
            Nothing -> do
                -- Skia's doc says:
                --
                -- Duplicates this stream. If this cannot be done, returns NULL.
                -- The returned stream will be positioned the same as this
                -- stream.
                pure nullPtr
            Just newStream -> do
                disownObject newStream

    fDestroy <- mkFunPtr'Sk_managedstream_destroy_proc $ \_ ctx -> do
        params <- interpretCtx ctx
        params.close

    sk_managedstream_set_procs Sk_managedstream_procs{..}
  where
    interpretCtx :: Ptr () -> IO StreamParams
    interpretCtx ptr = deRefStablePtr (castPtrToStablePtr ptr)

destroy :: (MonadIO m) => SKManagedStream -> m ()
destroy stream = evalContIO do
    stream' <- useObj stream
    liftIO $ sk_managedstream_destroy stream'

createFromParams :: (MonadIO m) => StreamParams -> m SKManagedStream
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

    stream' <- sk_managedstream_new (castStablePtrToPtr params')
    toObject stream'

-- | NOTE: This function will take the ownership of the input 'Handle'.
createFromHandle :: (MonadIO m) => Handle -> m SKManagedStream
createFromHandle handle = liftIO do
    let
        trySeek :: SeekMode -> Int -> IO Bool
        trySeek seekMode position = do
            seekable <- hIsSeekable handle
            if seekable
                then do
                    hSeek handle seekMode (fromIntegral position)
                    pure True
                else do
                    pure False

    createFromParams
        StreamParams
            { read = hGetBufSome handle
            , peek = \_buffer _peekSize -> do
                -- TODO: Is there a way to implement this?
                pure 0
            , isAtEnd = hIsEOF handle
            , hasPosition = hIsSeekable handle
            , getPosition = fromIntegral <$> hTell handle
            , hasLength = hIsSeekable handle
            , getLength = fromIntegral <$> hFileSize handle
            , rewind = trySeek AbsoluteSeek 0
            , seek = trySeek AbsoluteSeek
            , move = trySeek RelativeSeek
            , duplicate = do
                -- TODO:
                undefined
            , fork = do
                -- TODO:
                undefined
            , close = hClose handle
            }
