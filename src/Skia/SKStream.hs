module Skia.SKStream (
    destroy,
    isAtEnd,
    rewind,
    readInt8,
    readInt16,
    readInt32,
    readWord8,
    readWord16,
    readWord32,
    readBool,
    hasLength,
    getLength,
    hasPosition,
    getPosition,
    readToBuffer,
    readBytes,
    peekToBuffer,
    peekBytes,
    seek,
    move,
    skip,
    fork,
    duplicate,
    withMemoryBase,
)
where

import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BS
import Skia.Internal.Prelude

destroy :: (IsSKStream stream, MonadIO m) => stream -> m ()
destroy (toA SKStream -> stream) = evalContIO do
    stream' <- useObj stream
    liftIO $ sk_stream_destroy stream'

-- | PRIVATE FUNCTION
privReadStorable ::
    (MonadIO m, Storable s, IsSKStream stream) =>
    (Ptr Sk_stream -> Ptr s -> IO CBool) ->
    stream ->
    m (Maybe s)
privReadStorable readFunc (toA SKStream -> stream) = evalContIO do
    stream' <- useObj stream
    result' <- useAlloca

    success <- liftIO $ toBool <$> readFunc stream' result'
    if success
        then liftIO $ Just <$> peek result'
        else pure Nothing

readInt8 :: (MonadIO m, IsSKStream stream) => stream -> m (Maybe Int8)
readInt8 = privReadStorable sk_stream_read_s8

readInt16 :: (MonadIO m, IsSKStream stream) => stream -> m (Maybe Int16)
readInt16 = privReadStorable sk_stream_read_s16

readInt32 :: (MonadIO m, IsSKStream stream) => stream -> m (Maybe Int32)
readInt32 = privReadStorable sk_stream_read_s32

readWord8 :: (MonadIO m, IsSKStream stream) => stream -> m (Maybe Word8)
readWord8 = privReadStorable sk_stream_read_u8

readWord16 :: (MonadIO m, IsSKStream stream) => stream -> m (Maybe Word16)
readWord16 = privReadStorable sk_stream_read_u16

readWord32 :: (MonadIO m, IsSKStream stream) => stream -> m (Maybe Word32)
readWord32 = privReadStorable sk_stream_read_u32

readBool :: (MonadIO m, IsSKStream stream) => stream -> m (Maybe Bool)
readBool stream = do
    r <- privReadStorable sk_stream_read_bool stream
    pure $ toBool <$> r

readToBuffer ::
    (MonadIO m, IsSKStream stream) =>
    stream ->
    -- Buffer
    Ptr Word8 ->
    -- Maximum number of bytes to read
    Int ->
    -- Read size
    m Int
readToBuffer (toA SKStream -> stream) buffer maxSize = evalContIO do
    stream' <- useObj stream
    readSize <- liftIO $ sk_stream_read stream' (castPtr buffer) (fromIntegral maxSize)
    pure $ fromIntegral readSize

readBytes ::
    (MonadIO m, IsSKStream stream) =>
    stream ->
    -- Maximum number of bytes to read
    Int ->
    m BS.ByteString
readBytes (toA SKStream -> stream) maxSize = do
    -- Code reference:
    -- https://hackage-content.haskell.org/package/bytestring-0.12.2.0/docs/Data-ByteString.html#v:hGet
    liftIO $ BS.createAndTrim maxSize \buffer -> readToBuffer stream buffer maxSize

peekToBuffer ::
    (MonadIO m, IsSKStream stream) =>
    stream ->
    -- Buffer
    Ptr Word8 ->
    -- Maximum number of bytes to peek
    Int ->
    -- peek size
    m Int
peekToBuffer (toA SKStream -> stream) buffer maxSize = evalContIO do
    stream' <- useObj stream
    peekSize <- liftIO $ sk_stream_peek stream' (castPtr buffer) (fromIntegral maxSize)
    pure $ fromIntegral peekSize

peekBytes ::
    (MonadIO m, IsSKStream stream) =>
    stream ->
    -- Maximum number of bytes to read
    Int ->
    m BS.ByteString
peekBytes (toA SKStream -> stream) maxSize = do
    -- Code reference:
    -- https://hackage-content.haskell.org/package/bytestring-0.12.2.0/docs/Data-ByteString.html#v:hGet
    liftIO $ BS.createAndTrim maxSize \buffer -> readToBuffer stream buffer maxSize

isAtEnd :: (IsSKStream stream, MonadIO m) => stream -> m Bool
isAtEnd (toA SKStream -> stream) = evalContIO do
    stream' <- useObj stream
    liftIO $ toBool <$> sk_stream_is_at_end stream'

hasPosition :: (IsSKStream stream, MonadIO m) => stream -> m Bool
hasPosition (toA SKStream -> stream) = evalContIO do
    stream' <- useObj stream
    liftIO $ toBool <$> sk_stream_has_position stream'

getPosition :: (IsSKStream stream, MonadIO m) => stream -> m Int
getPosition (toA SKStream -> stream) = evalContIO do
    stream' <- useObj stream
    liftIO $ fromIntegral <$> sk_stream_get_position stream'

hasLength :: (IsSKStream stream, MonadIO m) => stream -> m Bool
hasLength (toA SKStream -> stream) = evalContIO do
    stream' <- useObj stream
    liftIO $ toBool <$> sk_stream_has_length stream'

getLength :: (IsSKStream stream, MonadIO m) => stream -> m Int
getLength (toA SKStream -> stream) = evalContIO do
    stream' <- useObj stream
    liftIO $ fromIntegral <$> sk_stream_get_length stream'

withMemoryBase :: (IsSKStream stream, MonadIO m) => stream -> (Ptr Word8 -> IO r) -> m r
withMemoryBase (toA SKStream -> stream) f = evalContIO do
    stream' <- useObj stream
    addr <- liftIO $ castPtr <$> sk_stream_get_memory_base stream'
    liftIO $ f addr

fork :: (IsSKStream stream, MonadIO m) => stream -> m SKStream
fork (toA SKStream -> stream) = evalContIO do
    stream' <- useObj stream
    newStream' <- liftIO $ sk_stream_fork stream'
    toObject newStream'

duplicate :: (IsSKStream stream, MonadIO m) => stream -> m SKStream
duplicate (toA SKStream -> stream) = evalContIO do
    stream' <- useObj stream
    newStream' <- liftIO $ sk_stream_duplicate stream'
    toObject newStream'

seek ::
    (IsSKStream stream, MonadIO m) =>
    stream ->
    -- | Position
    Int ->
    m Bool
seek (toA SKStream -> stream) pos = evalContIO do
    stream' <- useObj stream
    liftIO $ toBool <$> sk_stream_seek stream' (fromIntegral pos)

skip ::
    (IsSKStream stream, MonadIO m) =>
    stream ->
    -- | Count
    Int ->
    m Int
skip (toA SKStream -> stream) count = evalContIO do
    stream' <- useObj stream
    liftIO $ fromIntegral <$> sk_stream_skip stream' (fromIntegral count)

move ::
    (IsSKStream stream, MonadIO m) =>
    stream ->
    -- | Offset
    Int ->
    m Bool
move (toA SKStream -> stream) offset = evalContIO do
    stream' <- useObj stream
    liftIO $ toBool <$> sk_stream_move stream' (fromIntegral offset)

rewind :: (IsSKStream stream, MonadIO m) => stream -> m Bool
rewind (toA SKStream -> stream) = evalContIO do
    stream' <- useObj stream
    liftIO $ toBool <$> sk_stream_rewind stream'
