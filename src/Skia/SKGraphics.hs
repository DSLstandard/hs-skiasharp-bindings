module Skia.SKGraphics where

import Skia.Internal.Prelude

initialize :: (MonadIO m) => m ()
initialize = liftIO do
    sk_graphics_init

{- | Return the max number of bytes that should be used by the font cache. If
the cache needs to allocate more, it will purge previous entries. This max can
be changed by calling 'setFontCacheLimit'.
-}
getFontCacheLimit :: (MonadIO m) => m Int
getFontCacheLimit = liftIO do
    fromIntegral <$> sk_graphics_get_font_cache_limit

{- | Specify the max number of bytes that should be used by the font cache. If
 the cache needs to allocate more, it will purge previous entries.

 This function returns the previous setting, as if 'getFontCacheLimit' had be
 called before the new limit was set.
-}
setFontCacheLimit ::
    (MonadIO m) =>
    -- | Number of bytes
    Int ->
    IO Int
setFontCacheLimit bytes = liftIO do
    fromIntegral <$> sk_graphics_set_font_cache_limit (fromIntegral bytes)

-- | Return the number of bytes currently used by the font cache.
getFontCacheUsed :: (MonadIO m) => m Int
getFontCacheUsed = liftIO do
    fromIntegral <$> sk_graphics_get_font_cache_used

{- | Return the number of entries in the font cache. A cache "entry" is
associated with each typeface + pointSize + matrix.
-}
getFontCacheCountUsed :: (MonadIO m) => m Int
getFontCacheCountUsed = liftIO do
    fromIntegral <$> sk_graphics_get_font_cache_count_used

{- | Return the current limit to the number of entries in the font cache. A
cache "entry" is associated with each typeface + pointSize + matrix.
-}
getFontCacheCountLimit :: (MonadIO m) => m Int
getFontCacheCountLimit = liftIO do
    fromIntegral <$> sk_graphics_get_font_cache_count_limit

{- | Set the limit to the number of entries in the font cache, and return the
previous value. If this new value is lower than the previous, it will
automatically try to purge entries to meet the new limit.
-}
setFontCacheCountLimit ::
    (MonadIO m) =>
    -- | Count
    Int ->
    IO Int
setFontCacheCountLimit count = liftIO do
    fromIntegral <$> sk_graphics_set_font_cache_count_limit (fromIntegral count)

{- | This function returns the memory used for temporary images and other
resources.
-}
getResourceCacheTotalBytesUsed :: (MonadIO m) => m Int
getResourceCacheTotalBytesUsed = liftIO do
    fromIntegral <$> sk_graphics_get_resource_cache_total_bytes_used

{- | Returns the memory usage limit for the resource cache, used for temporary
bitmaps and other resources. Entries are purged from the cache when the memory
usage exceeds this limit.
-}
getResourceCacheTotalByteLimit :: (MonadIO m) => m Int
getResourceCacheTotalByteLimit = liftIO do
    fromIntegral <$> sk_graphics_get_resource_cache_total_byte_limit

{- | Sets the memory usage limit for the resource cache, used for temporary
bitmaps and other resources. The previous limit value is returned as though
'getResourceCacheTotalByteLimit' was called before calling
'setResourceCacheTotalByteLimit'.
-}
setResourceCacheTotalByteLimit ::
    (MonadIO m) =>
    -- | New limit
    Int ->
    m Int
setResourceCacheTotalByteLimit newLimit = liftIO do
    fromIntegral <$> sk_graphics_set_resource_cache_total_byte_limit (fromIntegral newLimit)

{- | When the cachable entry is very lage (e.g. a large scaled bitmap), adding
it to the cache can cause most/all of the existing entries to be purged. To
avoid the, the client can set a limit for a single allocation. If a cacheable
entry would have been cached, but its size exceeds this limit, then we do not
attempt to cache it at all.

Zero is the default value, meaning we always attempt to cache entries.
-}
getResourceCacheSingleAllocationByteLimit :: (MonadIO m) => m Int
getResourceCacheSingleAllocationByteLimit = liftIO do
    fromIntegral <$> sk_graphics_get_resource_cache_single_allocation_byte_limit

{- | The setter for 'getResourceCacheSingleAllocationByteLimit'. Returns the
previous limit.
-}
setResourceCacheSingleAllocationByteLimit ::
    (MonadIO m) =>
    -- | New limit
    Int ->
    m Int
setResourceCacheSingleAllocationByteLimit newLimit = liftIO do
    fromIntegral <$> sk_graphics_set_resource_cache_single_allocation_byte_limit (fromIntegral newLimit)

-- | Dumps memory usage of caches using the 'SKTraceMemoryDump' interface
dumpMemoryStatistics :: (MonadIO m, IsSKTraceMemoryDump dump) => dump -> m ()
dumpMemoryStatistics (toA SKTraceMemoryDump -> dump) = evalContIO do
    dump' <- useObj dump
    liftIO $ sk_graphics_dump_memory_statistics dump'

{- | For debugging purposes, this will attempt to purge the font cache. It does
not change the limit, but will cause subsequent font measures and draws to be
recreated, since they will no longer be in the cache.
-}
purgeFontCache :: (MonadIO m) => m ()
purgeFontCache = liftIO do
    sk_graphics_purge_font_cache

{- | For debugging purposes, this will attempt to purge the resource cache. It
does not change the limit.
-}
purgeResourceCache :: (MonadIO m) => m ()
purgeResourceCache = liftIO do
    sk_graphics_purge_resource_cache

{- | Free as much globally cached memory as possible. This will purge all
private caches in Skia, including font and image caches.

If there are caches associated with GPU context, those will not be affected by
this call.
-}
purgeAllCaches :: (MonadIO m) => m ()
purgeAllCaches = liftIO do
    sk_graphics_purge_all_caches
