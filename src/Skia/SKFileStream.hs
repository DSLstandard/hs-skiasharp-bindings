module Skia.SKFileStream where

import Foreign
import Skia.Internal.Prelude

destroy :: (MonadIO m) => SKFileStream -> m ()
destroy stream = evalContIO do
    stream' <- useObj stream
    liftIO $ sk_filestream_destroy stream'

create :: (MonadIO m) => FilePath -> m (Maybe SKFileStream)
create path = liftIO do
    withCString path \path' -> do
        stream' <- sk_filestream_new path'
        toObjectMaybe stream'

isValid :: (MonadIO m) => SKFileStream -> m Bool
isValid stream = evalContIO do
    stream' <- useObj stream
    liftIO $ toBool <$> sk_filestream_is_valid stream'
