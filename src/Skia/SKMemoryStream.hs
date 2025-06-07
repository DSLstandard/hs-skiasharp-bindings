module Skia.SKMemoryStream where

import Skia.Internal.Prelude

destroy :: (MonadIO m) => SKMemoryStream -> m ()
destroy stream = evalContIO do
    stream' <- useObj stream
    liftIO $ sk_memorystream_destroy stream'

create :: (MonadIO m) => m (Maybe SKMemoryStream)
create = liftIO do
    stream' <- sk_memorystream_new
    toObjectMaybe stream'

createWithLength :: (MonadIO m) => Int -> m (Maybe SKMemoryStream)
createWithLength len = liftIO do
    stream' <- liftIO $ sk_memorystream_new_with_length (fromIntegral len)
    toObjectMaybe stream'

createWithData ::
    (MonadIO m) =>
    -- | Buffer
    Ptr Word8 ->
    -- | Buffer size
    Int ->
    -- | Copy data?
    Bool ->
    m (Maybe SKMemoryStream)
createWithData buffer size copyData = liftIO do
    stream' <- liftIO $ sk_memorystream_new_with_data (castPtr buffer) (fromIntegral size) (fromBool copyData)
    toObjectMaybe stream'

createWithSKData :: (MonadIO m) => SKData -> m (Maybe SKMemoryStream)
createWithSKData dat = evalContIO do
    dat' <- useObj dat
    stream' <- liftIO $ sk_memorystream_new_with_skdata dat'
    toObjectMaybe stream'

setMemory ::
    (MonadIO m) =>
    SKMemoryStream ->
    -- | Buffer
    Ptr Word8 ->
    -- | Buffer size
    Int ->
    -- | Copy data?
    Bool ->
    m ()
setMemory stream buffer size copyData = evalContIO do
    stream' <- useObj stream
    liftIO $ sk_memorystream_set_memory stream' (castPtr buffer) (fromIntegral size) (fromBool copyData)
