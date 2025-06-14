{- | SKString

Light weight class for managing strings. Uses reference counting to make
string assignments and copies very fast with no extra RAM cost. Assumes UTF8
encoding.
-}
module Skia.SKString where

import Data.Text qualified as T
import Data.Text.Foreign qualified as T
import Foreign.C.String qualified
import Skia.Internal.Prelude

{- | Creates an empty SKString

The 'SKString' will be destroyed automatically upon being finalized by Haskell,
but you may use 'disposeObject' to free up the resources used by 'SKString'
immediately.
-}
createEmpty :: (MonadIO m) => m SKString
createEmpty = liftIO do
    str <- sk_string_new_empty
    toObjectFin sk_string_destructor str

{- | Creates an SKString by copying from a 'CStringLen'. Assumes UTF-8 encoding.

The 'SKString' will be destroyed automatically upon being finalized by Haskell,
but you may use 'disposeObject' to free up the resources used by 'SKString'
immediately.
-}
createFromCStringLen :: (MonadIO m) => CStringLen -> m SKString
createFromCStringLen (ptr, len) = liftIO do
    str <- sk_string_new_with_copy ptr (fromIntegral len)
    toObjectFin sk_string_destructor str

{- | Creates an SKString by copying from a 'T.Text'.

The 'SKString' will be destroyed automatically upon being finalized by Haskell,
but you may use 'disposeObject' to free up the resources used by 'SKString'
immediately.
-}
createFromText :: (MonadIO m) => T.Text -> m SKString
createFromText txt = liftIO do
    -- TODO: Make it more efficient if possible. T.withCString seems to take
    -- O(n) time to marshal.
    T.withCStringLen txt createFromCStringLen

{- | Creates an SKString by copying from a 'String'.

The 'SKString' will be destroyed automatically upon being finalized by Haskell,
but you may use 'disposeObject' to free up the resources used by 'SKString'
immediately.
-}
createFromString :: (MonadIO m) => String -> m SKString
createFromString string = liftIO do
    Foreign.C.String.withCStringLen string createFromCStringLen

-- | Returns the number of bytes of the SKString.
getSize :: (MonadIO m) => SKString -> m Int
getSize str = evalContIO do
    str' <- useObj str
    liftIO $ fromIntegral <$> sk_string_get_size str'

-- | Uses the data pointer of the SKString. The encoding is UTF-8.
withData :: (MonadIO m) => SKString -> (Ptr CChar -> IO r) -> m r
withData str f = evalContIO do
    str' <- useObj str
    cstr <- liftIO $ sk_string_get_c_str str'
    liftIO $ f cstr

getAsText :: (MonadIO m) => SKString -> m T.Text
getAsText str =
    withData str \ptr -> do
        len <- getSize str
        liftIO $ T.peekCStringLen (ptr, len)

getAsString :: (MonadIO m) => SKString -> m String
getAsString str =
    withData str \ptr -> do
        len <- getSize str
        liftIO $ Foreign.C.String.peekCStringLen (ptr, len)