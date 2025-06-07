module Skia.SKWStream where

import Skia.Internal.Prelude

getBytesWritten :: (IsSKWStream stream, MonadIO m) => stream -> m Int
getBytesWritten (toA SKWStream -> stream) = evalContIO do
    stream' <- useObj stream
    liftIO $ fromIntegral <$> sk_wstream_bytes_written stream'

writeBuffer ::
    (IsSKWStream stream, MonadIO m) =>
    stream ->
    -- | Buffer
    Ptr Word8 ->
    -- | Buffer size
    Int ->
    m Bool
writeBuffer (toA SKWStream -> stream) buffer sz = evalContIO do
    stream' <- useObj stream
    liftIO $ toBool <$> sk_wstream_write stream' (castPtr buffer) (fromIntegral sz)

newline :: (IsSKWStream stream, MonadIO m) => stream -> m Bool
newline (toA SKWStream -> stream) = evalContIO do
    stream' <- useObj stream
    liftIO $ toBool <$> sk_wstream_newline stream'

flush :: (IsSKWStream stream, MonadIO m) => stream -> m ()
flush (toA SKWStream -> stream) = evalContIO do
    stream' <- useObj stream
    liftIO $ sk_wstream_flush stream'

writeWord8 :: (MonadIO m, IsSKWStream stream) => stream -> Word8 -> m Bool
writeWord8 (toA SKWStream -> stream) x = evalContIO do
    stream' <- useObj stream
    liftIO $ toBool <$> sk_wstream_write_8 stream' x

writeWord16 :: (MonadIO m, IsSKWStream stream) => stream -> Word16 -> m Bool
writeWord16 (toA SKWStream -> stream) x = evalContIO do
    stream' <- useObj stream
    liftIO $ toBool <$> sk_wstream_write_16 stream' x

writeWord32 :: (MonadIO m, IsSKWStream stream) => stream -> Word32 -> m Bool
writeWord32 (toA SKWStream -> stream) x = evalContIO do
    stream' <- useObj stream
    liftIO $ toBool <$> sk_wstream_write_32 stream' x

writeScalar :: (MonadIO m, IsSKWStream stream) => stream -> Float -> m Bool
writeScalar (toA SKWStream -> stream) value = evalContIO do
    stream' <- useObj stream
    liftIO $ toBool <$> sk_wstream_write_scalar stream' (coerce value)

writeBool :: (MonadIO m, IsSKWStream stream) => stream -> Bool -> m Bool
writeBool (toA SKWStream -> stream) x = evalContIO do
    stream' <- useObj stream
    liftIO $ toBool <$> sk_wstream_write_bool stream' (fromBool x)

writePackedUInt :: (MonadIO m, IsSKWStream stream) => stream -> CSize -> m Bool
writePackedUInt (toA SKWStream -> stream) value = evalContIO do
    stream' <- useObj stream
    liftIO $ toBool <$> sk_wstream_write_packed_uint stream' value

writeCString :: (MonadIO m, IsSKWStream stream) => stream -> CString -> m Bool
writeCString (toA SKWStream -> stream) string = evalContIO do
    stream' <- useObj stream
    liftIO $ toBool <$> sk_wstream_write_text stream' string

writeDecimalAsText :: (MonadIO m, IsSKWStream stream) => stream -> Int32 -> m Bool
writeDecimalAsText (toA SKWStream -> stream) value = evalContIO do
    stream' <- useObj stream
    liftIO $ toBool <$> sk_wstream_write_dec_as_text stream' value

writeBigDecimalAsText ::
    (MonadIO m, IsSKWStream stream) =>
    stream ->
    Int64 ->
    -- | Min digits
    Int ->
    m Bool
writeBigDecimalAsText (toA SKWStream -> stream) value minDigits = evalContIO do
    stream' <- useObj stream
    liftIO $ toBool <$> sk_wstream_write_bigdec_as_text stream' value (fromIntegral minDigits)

writeHexAsText ::
    (MonadIO m, IsSKWStream stream) =>
    stream ->
    Word32 ->
    -- | Min digits
    Int ->
    m Bool
writeHexAsText (toA SKWStream -> stream) value minDigits = evalContIO do
    stream' <- useObj stream
    liftIO $ toBool <$> sk_wstream_write_hex_as_text stream' (coerce value) (fromIntegral minDigits)

writeScalarAsText :: (MonadIO m, IsSKWStream stream) => stream -> Float -> m Bool
writeScalarAsText (toA SKWStream -> stream) value = evalContIO do
    stream' <- useObj stream
    liftIO $ toBool <$> sk_wstream_write_scalar_as_text stream' (coerce value)

writeStream ::
    (MonadIO m, IsSKWStream stream, IsSKStream input) =>
    stream ->
    -- | Input
    input ->
    -- | Length
    Int ->
    m Bool
writeStream (toA SKWStream -> stream) (toA SKStream -> input) len = evalContIO do
    stream' <- useObj stream
    input' <- useObj input
    liftIO $ toBool <$> sk_wstream_write_stream stream' input' (fromIntegral len)

getSizeOfPackedUInt :: (MonadIO m) => CSize -> m Int
getSizeOfPackedUInt value = do
    liftIO $ fromIntegral <$> sk_wstream_get_size_of_packed_uint value
