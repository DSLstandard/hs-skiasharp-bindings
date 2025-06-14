module Skia.SKFileWStream where

import Skia.Internal.Prelude

destroy :: (MonadIO m) => SKFileWStream -> m ()
destroy stream = evalContIO do
    stream' <- useObj stream
    liftIO $ sk_filewstream_destroy stream'

create :: (MonadIO m) => FilePath -> m (Maybe SKFileWStream)
create path = liftIO do
    withCString path \path' -> do
        stream' <- sk_filewstream_new path'
        toObjectMaybe stream'

isValid :: (MonadIO m) => SKFileWStream -> m Bool
isValid stream = evalContIO do
    stream' <- useObj stream
    liftIO $ toBool <$> sk_filewstream_is_valid stream'
