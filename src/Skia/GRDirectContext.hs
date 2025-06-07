module Skia.GRDirectContext (
    abandon,
    isAbandoned,
    ContextOptions (..),
    createGl,
    createVulkan,
    createMetal,
    createDirect3D,
    flush,
    submit,
    flushAndSubmit,
    flushImage,
    flushSurface,
    BackendState,
    backendState'None,
    backendState'All,
    resetContextByState,
    resetContextAll,
    dumpMemoryStatistics,
    freeGPUResources,
    performDeferredCleanup,
    purgeUnlockedResourcesBytes,
    purgeUnlockedResources,
    getResourceCacheLimit,
    setResourceCacheLimit,
    getResourceCacheUsage,
)
where

import Skia.Internal.Prelude

abandon ::
    (MonadIO m) =>
    GRDirectContext ->
    -- | Release resources too?
    Bool ->
    m ()
abandon ctx releaseResources = evalContIO do
    ctx' <- useObj ctx
    if releaseResources
        then liftIO $ gr_direct_context_release_resources_and_abandon_context ctx'
        else liftIO $ gr_direct_context_abandon_context ctx'

isAbandoned :: (MonadIO m) => GRDirectContext -> m Bool
isAbandoned ctx = evalContIO do
    ctx' <- useObj ctx
    liftIO $ fmap toBool $ gr_direct_context_is_abandoned ctx'

data ContextOptions = ContextOptions
    { avoidStencilBuffers :: Bool
    , runtimeProgramCacheSize :: Int
    , glyphCacheTextureMaximumBytes :: Int
    , allowPathMaskCaching :: Bool
    , doManualMipmapping :: Bool
    , bufferMapThreshold :: Int
    }
    deriving (Show, Eq, Ord)

marshalContextOptions :: ContextOptions -> Gr_context_options
marshalContextOptions i =
    Gr_context_options
        { fAvoidStencilBuffers = fromBool i.avoidStencilBuffers
        , fRuntimeProgramCacheSize = fromIntegral i.runtimeProgramCacheSize
        , fGlyphCacheTextureMaximumBytes = fromIntegral i.glyphCacheTextureMaximumBytes
        , fAllowPathMaskCaching = fromBool i.allowPathMaskCaching
        , fDoManualMipmapping = fromBool i.doManualMipmapping
        , fBufferMapThreshold = fromIntegral i.bufferMapThreshold
        }

privCreateHelper ::
    (MonadIO m) =>
    IO (Ptr Gr_direct_context) ->
    (Ptr Gr_context_options -> IO (Ptr Gr_direct_context)) ->
    Maybe ContextOptions ->
    m GRDirectContext
privCreateHelper createWithoutOptions createWithOptions options = evalContIO do
    ctx' <- case options of
        Nothing -> do
            liftIO createWithoutOptions
        Just options -> do
            options' <- useStorable (marshalContextOptions options)
            liftIO $ createWithOptions options'
    toObjectFin (gr_recording_context_unref . pointerCast GRDirectContext GRRecordingContext) ctx'

createGl :: (MonadIO m) => GRGlInterface -> Maybe ContextOptions -> m GRDirectContext
createGl iface opts = evalContIO do
    iface' <- useObj iface
    privCreateHelper
        (gr_direct_context_make_gl iface')
        (gr_direct_context_make_gl_with_options iface')
        opts

createVulkan :: (MonadIO m) => Gr_vk_backendcontext -> Maybe ContextOptions -> m GRDirectContext
createVulkan vkbackend opts = evalContIO do
    privCreateHelper
        (gr_direct_context_make_vulkan vkbackend)
        (gr_direct_context_make_vulkan_with_options vkbackend)
        opts

createMetal ::
    (MonadIO m) =>
    -- | Device
    Ptr () ->
    -- | Queue
    Ptr () ->
    Maybe ContextOptions ->
    m GRDirectContext
createMetal device queue opts = evalContIO do
    privCreateHelper
        (gr_direct_context_make_metal device queue)
        (gr_direct_context_make_metal_with_options device queue)
        opts

createDirect3D :: (MonadIO m) => Gr_d3d_backendcontext -> Maybe ContextOptions -> m GRDirectContext
createDirect3D d3dbackend opts = evalContIO do
    privCreateHelper
        (gr_direct_context_make_direct3d d3dbackend)
        (gr_direct_context_make_direct3d_with_options d3dbackend)
        opts

getResourceCacheLimit :: (MonadIO m) => GRDirectContext -> m Int
getResourceCacheLimit ctx = evalContIO do
    ctx' <- useObj ctx
    liftIO $ fmap fromIntegral $ gr_direct_context_get_resource_cache_limit ctx'

setResourceCacheLimit :: (MonadIO m) => GRDirectContext -> Int -> m ()
setResourceCacheLimit ctx limit = evalContIO do
    ctx' <- useObj ctx
    liftIO $ gr_direct_context_set_resource_cache_limit ctx' (fromIntegral limit)

data CacheUsage = CacheUsage
    { maxResources :: Int
    , maxResourcesBytes :: Int
    }
    deriving (Show, Eq, Ord)

getResourceCacheUsage :: (MonadIO m) => GRDirectContext -> m CacheUsage
getResourceCacheUsage ctx = evalContIO do
    ctx' <- useObj ctx

    maxResources' <- useAlloca
    maxResourcesBytes' <- useAlloca

    liftIO $ gr_direct_context_get_resource_cache_usage ctx' maxResources' maxResourcesBytes'

    maxResources <- peekWith fromIntegral maxResources'
    maxResourcesBytes <- peekWith fromIntegral maxResourcesBytes'
    pure CacheUsage{maxResources, maxResourcesBytes}

flush :: (MonadIO m) => GRDirectContext -> m ()
flush ctx = evalContIO do
    ctx' <- useObj ctx
    liftIO $ gr_direct_context_flush ctx'

submit ::
    (MonadIO m) =>
    GRDirectContext ->
    -- | Synchronous?
    Bool ->
    m Bool
submit ctx sync = evalContIO do
    ctx' <- useObj ctx
    liftIO $ fmap toBool $ gr_direct_context_submit ctx' (fromBool sync)

flushAndSubmit ::
    (MonadIO m) =>
    GRDirectContext ->
    -- | Synchronous?
    Bool ->
    m ()
flushAndSubmit ctx sync = evalContIO do
    ctx' <- useObj ctx
    liftIO $ gr_direct_context_flush_and_submit ctx' (fromBool sync)

flushImage :: (MonadIO m) => GRDirectContext -> SKImage -> m ()
flushImage ctx image = evalContIO do
    ctx' <- useObj ctx
    image' <- useObj image
    liftIO $ gr_direct_context_flush_image ctx' image'

flushSurface :: (MonadIO m) => GRDirectContext -> SKSurface -> m ()
flushSurface ctx surface = evalContIO do
    ctx' <- useObj ctx
    surface' <- useObj surface
    liftIO $ gr_direct_context_flush_surface ctx' surface'

type BackendState = Word32

backendState'None :: BackendState
backendState'None = 0x0

backendState'All :: BackendState
backendState'All = 0xFFFFFFFF

resetContextByState :: (MonadIO m) => GRDirectContext -> BackendState -> m ()
resetContextByState ctx state = evalContIO do
    ctx' <- useObj ctx
    liftIO $ gr_direct_context_reset_context ctx' state

resetContextAll :: (MonadIO m) => GRDirectContext -> m ()
resetContextAll ctx = resetContextByState ctx backendState'All

dumpMemoryStatistics :: (MonadIO m) => GRDirectContext -> SKTraceMemoryDump -> m ()
dumpMemoryStatistics ctx dump = evalContIO do
    ctx' <- useObj ctx
    dump' <- useObj dump
    liftIO $ gr_direct_context_dump_memory_statistics ctx' dump'

freeGPUResources :: (MonadIO m) => GRDirectContext -> m ()
freeGPUResources ctx = evalContIO do
    ctx' <- useObj ctx
    liftIO $ gr_direct_context_free_gpu_resources ctx'

performDeferredCleanup ::
    (MonadIO m) =>
    GRDirectContext ->
    -- | Delay in ms
    Int ->
    m ()
performDeferredCleanup ctx delayMs = evalContIO do
    ctx' <- useObj ctx
    liftIO $ gr_direct_context_perform_deferred_cleanup ctx' (fromIntegral delayMs)

purgeUnlockedResourcesBytes ::
    (MonadIO m) =>
    GRDirectContext ->
    -- | Number of bytes to purge
    Int ->
    -- | Prefer scratch resources?
    Bool ->
    m ()
purgeUnlockedResourcesBytes ctx bytesToPurge preferScratchResources = evalContIO do
    ctx' <- useObj ctx
    liftIO $ gr_direct_context_purge_unlocked_resources_bytes ctx' (fromIntegral bytesToPurge) (fromBool preferScratchResources)

purgeUnlockedResources ::
    (MonadIO m) =>
    GRDirectContext ->
    -- | Scratch resources only?
    Bool ->
    m ()
purgeUnlockedResources ctx scratchResourcesOnly = evalContIO do
    ctx' <- useObj ctx
    liftIO $ gr_direct_context_purge_unlocked_resources ctx' (fromBool scratchResourcesOnly)
