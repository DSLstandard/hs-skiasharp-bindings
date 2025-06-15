module Skia.SKFileStream where

import Control.Exception
import Foreign
import Skia.Internal.Prelude

{- | Creates a new 'SKFileStream' that wraps the file with the specified path.

You need to use 'isValid' to check validity of the returned 'SKFileStream'.
-}
createFromFile :: FilePath -> Acquire SKFileStream
createFromFile path =
    mkSKObjectAcquire
        ( do
            stream' <- withCString path \path' -> do
                sk_filestream_new path'
            pure stream'
        )
        sk_filestream_destroy

-- | Returns true if the current path could be opened.
isValid :: (MonadIO m) => SKFileStream -> m Bool
isValid stream = evalContIO do
    stream' <- useObj stream
    liftIO $ toBool <$> sk_filestream_is_valid stream'
