module Skia.SKFileWStream where

import Skia.Internal.Prelude

-- | Creates a new 'SKFileWStream' that wraps the file with the specified path.
create :: FilePath -> Acquire SKFileWStream
create path =
    mkSKObjectAcquire
        ( withCString path \path' -> do
            sk_filewstream_new path'
        )
        sk_filewstream_destroy

-- | Returns true if the current path could be opened.
isValid :: (MonadIO m) => SKFileWStream -> m Bool
isValid stream = liftIO do
    toBool <$> sk_filewstream_is_valid (ptr stream)
