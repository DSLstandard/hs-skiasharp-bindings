module Skia.SKVersion where

import Skia.Internal.Prelude
import System.IO.Unsafe

getMilestone :: (MonadIO m) => m Int
getMilestone = liftIO $ fromIntegral <$> sk_version_get_milestone

getIncrement :: (MonadIO m) => m Int
getIncrement = liftIO $ fromIntegral <$> sk_version_get_increment

getVersionCString :: (MonadIO m) => m CString
getVersionCString = liftIO do
    sk_version_get_string

milestone :: Int
milestone = unsafeDupablePerformIO getMilestone
{-# NOINLINE milestone #-}

increment :: Int
increment = unsafeDupablePerformIO getIncrement
{-# NOINLINE increment #-}

versionCString :: CString
versionCString = unsafeDupablePerformIO getVersionCString
{-# NOINLINE versionCString #-}

versionString :: String
versionString = unsafePerformIO do
    cstring <- getVersionCString
    peekCString cstring
{-# NOINLINE versionString #-}
