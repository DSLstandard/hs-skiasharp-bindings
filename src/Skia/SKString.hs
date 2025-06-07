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

-- | Creates an empty SKString
createEmpty :: (MonadIO m) => m SKString
createEmpty = liftIO do
    str <- sk_string_new_empty
    toObjectFin sk_string_destructor str

-- | Creates an SKString by copying from a 'CStringLen'. Assumes UTF-8 encoding.
createFromCStringLen :: (MonadIO m) => CStringLen -> m SKString
createFromCStringLen (ptr, len) = liftIO do
    str <- sk_string_new_with_copy ptr (fromIntegral len)
    toObjectFin sk_string_destructor str

-- | Creates an SKString by copying from a 'T.Text'.
createFromText :: (MonadIO m) => T.Text -> m SKString
createFromText txt = liftIO do
    -- TODO: Make it more efficient if possible. T.withCString seems to take
    -- O(n) time to marshal.
    T.withCStringLen txt createFromCStringLen

-- | Creates an SKString by copying from a 'String'.
createFromString :: (MonadIO m) => String -> m SKString
createFromString string = liftIO do
    Foreign.C.String.withCStringLen string createFromCStringLen

-- | Returns the number of bytes of the SKString.
getSize :: (MonadIO m) => SKString -> m Int
getSize str = evalContIO do
    str' <- useObj str
    liftIO $ fromIntegral <$> sk_string_get_size str'

-- | Returns the data pointer of the SKString. The encoding is UTF-8.
getData :: (MonadIO m) => SKString -> m (Ptr CChar)
getData str = evalContIO do
    str' <- useObj str
    liftIO $ sk_string_get_c_str str'

-- | Returns a 'CStringLen' representation of the SKString. The encoding is UTF-8.
getCStringLen :: (MonadIO m) => SKString -> m CStringLen
getCStringLen str = do
    ptr <- getData str
    sz <- getSize str
    pure (ptr, sz)

getAsText :: (MonadIO m) => SKString -> m T.Text
getAsText str = liftIO $ getCStringLen str >>= T.peekCStringLen

getAsString :: (MonadIO m) => SKString -> m String
getAsString str = liftIO $ getCStringLen str >>= Foreign.C.String.peekCStringLen
