module Skia.SKAssetStream where

import Skia.Internal.Prelude

delete :: (MonadIO m) => SKAssetStream -> m ()
delete stream = evalContIO do
    stream' <- useObj stream
    liftIO $ sk_stream_asset_destroy stream'
