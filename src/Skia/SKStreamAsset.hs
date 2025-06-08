module Skia.SKStreamAsset where

import Skia.Internal.Prelude

destroy :: (MonadIO m) => SKStreamAsset -> m ()
destroy stream = evalContIO do
    stream' <- useObj stream
    liftIO $ sk_stream_asset_destroy stream'
