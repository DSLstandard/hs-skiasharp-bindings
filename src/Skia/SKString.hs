{- | SKString

Light weight class for managing strings. Uses reference counting to make
string assignments and copies very fast with no extra RAM cost. Assumes UTF8
encoding.
-}
module Skia.SKString where

import Data.Acquire qualified as Acquire
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Foreign qualified as T
import Foreign.C.String qualified
import Skia.Internal.Prelude

{- | Creates an empty SKString

When 'Acquire' releases, the returned 'SKString' is destroyed.
-}
createEmpty :: Acquire SKString
createEmpty =
    mkSKObjectAcquire
        sk_string_new_empty
        sk_string_destructor

{- | O(n). Creates an SKString by copying from a 'BS.ByteString'. Assumes UTF-8
encoding.

When 'Acquire' releases, the returned 'SKString' is destroyed.
-}
createFromByteString :: BS.ByteString -> Acquire SKString
createFromByteString bs =
    mkSKObjectAcquire
        ( evalContIO do
            (ptr, len) <- ContT $ BS.unsafeUseAsCStringLen bs
            liftIO $ sk_string_new_with_copy ptr (fromIntegral len)
        )
        sk_string_destructor

{- | O(n). Creates an SKString by copying from a 'T.Text'.

When 'Acquire' releases, the returned 'SKString' is destroyed.
-}
createFromText :: T.Text -> Acquire SKString
createFromText txt =
    -- TODO: Make it more efficient if possible. T.encodeUtf8 is awful.
    createFromByteString (T.encodeUtf8 txt)

{- | O(n). Creates an SKString by copying from a 'String'.

When 'Acquire' releases, the returned 'SKString' is destroyed.
-}
createFromString :: String -> Acquire SKString
createFromString string = createFromText (T.pack string)

-- | Returns the number of bytes of the SKString.
getSize :: (MonadIO m) => SKString -> m Int
getSize str = evalContIO do
    str' <- useObj str
    liftIO $ fromIntegral <$> sk_string_get_size str'

{- | Exposes the **read-only** data pointer of the SKString. The encoding is
UTF-8.

NOTE: When the 'SKString' if destructed, the returned @Ptr CChar@ is literally
dead.
-}
acquireData :: MonadIO m => SKString -> m (Ptr CChar)
acquireData str = liftIO do
    sk_string_get_c_str (ptr str)

-- | O(n). Builds a 'T.Text' by copying from the input 'SKString'.
getAsText :: (MonadIO m) => SKString -> m T.Text
getAsText str = liftIO do
    Acquire.with (acquireData str) \ptr -> do
        len <- getSize str
        T.peekCStringLen (ptr, len)

-- | Like 'getAsText' but returns 'String'.
getAsString :: (MonadIO m) => SKString -> m String
getAsString str = T.unpack <$> getAsText str
