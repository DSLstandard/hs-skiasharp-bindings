module Skia.GRDirectContext (
    -- * Creating 'GRDirectContext'
    ContextOptions (..),
    defaultContextOptions,
    createGl,
    createVulkan,
    createMetal,
    createDirect3D,

    -- * Utilities related to abandoning contexts
    abandonContext,
    releaseResourcesAndAbandonContext,
    isAbandoned,

    -- * Resource cache utilities
    getResourceCacheLimit,
    setResourceCacheLimit,
    getResourceCacheUsage,
    freeGPUResources,
    performDeferredCleanup,
    purgeUnlockedResourcesBytes,
    purgeUnlockedResources,

    -- * Utilities to flush and submit draw commands
    flush,
    submit,
    flushAndSubmit,
    flushImage,
    flushSurface,

    -- * @resetContext@
    BackendState,
    backendState'None,
    backendState'All,
    resetContext,
    resetContextAll,

    -- * Miscellaneous utilities
    dumpMemoryStatistics,
)
where

import Skia.Internal.Prelude
import Skia.SKRefCnt qualified as SKRefCnt

-- | Additional options used in constructing 'GRDirectContext'.
data ContextOptions = ContextOptions
    { avoidStencilBuffers :: Bool
    -- ^ Bugs on certain drivers cause stencil buffers to leak. This flag causes
    -- Skia to avoid allocating stencil buffers and use alternate rasterization
    -- paths, avoiding the leak.
    , runtimeProgramCacheSize :: Int
    -- ^ Maximum number of GPU programs or pipelines to keep active in the
    -- runtime cache.
    , glyphCacheTextureMaximumBytes :: Int
    -- ^ The maximum size of cache textures used for Skia's Glyph cache.
    , allowPathMaskCaching :: Bool
    -- ^ If true this allows path mask textures to be cached. This is only
    -- really useful if paths are commonly rendered at the same scale and
    -- fractional translation.
    , doManualMipmapping :: Bool
    -- ^ Construct mipmaps manually, via repeated downsampling draw-calls. This
    -- is used when the driver's implementation (glGenerateMipmap) contains
    -- bugs. This requires mipmap level control (ie desktop or ES3).
    , bufferMapThreshold :: Maybe Int
    -- ^ the threshold in bytes above which we will use a buffer mapping API to
    -- map vertex and index buffers to CPU memory in order to update them.
    -- Putting 'Nothing' here means the GrContext should deduce the optimal
    -- value for this platform.
    }
    deriving (Show, Eq, Ord)

-- | A 'ContextOptions' with Google Skia's default values.
defaultContextOptions :: ContextOptions
defaultContextOptions =
    -- See include/gpu/GrContextOptions.h
    ContextOptions
        { avoidStencilBuffers = False
        , runtimeProgramCacheSize = 256
        , glyphCacheTextureMaximumBytes = 2048 * 1024 * 4
        , allowPathMaskCaching = True
        , doManualMipmapping = False
        , bufferMapThreshold = Nothing
        }

marshalContextOptions :: ContextOptions -> Gr_context_options
marshalContextOptions i =
    Gr_context_options
        { fAvoidStencilBuffers = fromBool i.avoidStencilBuffers
        , fRuntimeProgramCacheSize = fromIntegral i.runtimeProgramCacheSize
        , fGlyphCacheTextureMaximumBytes = fromIntegral i.glyphCacheTextureMaximumBytes
        , fAllowPathMaskCaching = fromBool i.allowPathMaskCaching
        , fDoManualMipmapping = fromBool i.doManualMipmapping
        , fBufferMapThreshold = maybe (-1) fromIntegral i.bufferMapThreshold
        }

privCreateHelper ::
    -- | Function to create 'GRDirectContext' without 'ContextOptions'
    IO (Ptr Gr_direct_context) ->
    -- | Function to create 'GRDirectContext' *WITH* 'ContextOptions'
    (Ptr Gr_context_options -> IO (Ptr Gr_direct_context)) ->
    Maybe ContextOptions ->
    Acquire GRDirectContext
privCreateHelper createWithoutOptions createWithOptions options =
    mkSKObjectAcquire'
        ( evalContIO do
            case options of
                Nothing ->
                    liftIO createWithoutOptions
                Just options -> do
                    options' <- useStorable (marshalContextOptions options)
                    liftIO $ createWithOptions options'
        )
        SKRefCnt.decrement

{- | Creates a 'GRDirectContext' for an OpenGL context.

The reference counter of the returned 'GRDirectContext' is decremented when
'Acquire' releases.
-}
createGl :: GRGlInterface -> Maybe ContextOptions -> Acquire GRDirectContext
createGl iface opts =
    privCreateHelper
        (gr_direct_context_make_gl (ptr iface))
        (gr_direct_context_make_gl_with_options (ptr iface))
        opts

{- | Creates a 'GRDirectContext' for a Vulkan context.

The reference counter of the returned 'GRDirectContext' is decremented when
'Acquire' releases.
-}
createVulkan :: Gr_vk_backendcontext -> Maybe ContextOptions -> Acquire GRDirectContext
createVulkan vkbackend opts =
    privCreateHelper
        (gr_direct_context_make_vulkan vkbackend)
        (gr_direct_context_make_vulkan_with_options vkbackend)
        opts

{- | Creates a 'GRDirectContext' for a Metal context.

The reference counter of the returned 'GRDirectContext' is decremented when
'Acquire' releases.
-}
createMetal ::
    -- | Device
    Ptr () ->
    -- | Queue
    Ptr () ->
    Maybe ContextOptions ->
    Acquire GRDirectContext
createMetal device queue opts =
    privCreateHelper
        (gr_direct_context_make_metal device queue)
        (gr_direct_context_make_metal_with_options device queue)
        opts

{- | Creates a 'GRDirectContext' for a Direct3D context.

The reference counter of the returned 'GRDirectContext' is decremented when
'Acquire' releases.
-}
createDirect3D :: Gr_d3d_backendcontext -> Maybe ContextOptions -> Acquire GRDirectContext
createDirect3D d3dbackend opts =
    privCreateHelper
        (gr_direct_context_make_direct3d d3dbackend)
        (gr_direct_context_make_direct3d_with_options d3dbackend)
        opts

{- | Abandons all GPU resources and assumes the underlying backend 3D API
context is no longer usable. Call this if you have lost the associated GPU
context, and thus internal texture, buffer, etc. references/IDs are now invalid.
Calling this ensures that the destructors of the context and any of its created
resource objects will not make backend 3D API calls. Content rendered but not
previously flushed may be lost. After this function is called all subsequent
calls on the context will fail or be no-ops.

The typical use case for this function is that the underlying 3D context was
lost and further API calls may crash.

This call is not valid to be made inside ReleaseProcs passed into SkSurface or
SkImages. The call will simply fail (and assert in debug) if it is called while
inside a ReleaseProc.

For Vulkan, even if the device becomes lost, the VkQueue, VkDevice, or
VkInstance used to create the context must be kept alive even after abandoning
the context. Those objects must live for the lifetime of the context object
itself. The reason for this is so that we can continue to delete any outstanding
GrBackendTextures/RenderTargets which must be cleaned up even in a device lost
state.
-}
abandonContext :: (MonadIO m) => GRDirectContext -> m ()
abandonContext ctx = liftIO do
    gr_direct_context_abandon_context (ptr ctx)

{- | This is similar to 'abandonContext' however the underlying 3D context is
not yet lost and the context will cleanup all allocated resources before
returning. After returning it will assume that the underlying context may no
longer be valid.

The typical use case for this function is that the client is going to destroy
the 3D context but can't guarantee that context will be destroyed first (perhaps
because it may be ref'ed elsewhere by either the client or Skia objects).

For Vulkan, even if the device becomes lost, the VkQueue, VkDevice, or
VkInstance used to create the context must be alive before calling
releaseResourcesAndAbandonContext.
-}
releaseResourcesAndAbandonContext :: (MonadIO m) => GRDirectContext -> m ()
releaseResourcesAndAbandonContext ctx = liftIO do
    gr_direct_context_release_resources_and_abandon_context (ptr ctx)

{- | Returns true if the context was abandoned or if the backend specific
context has gotten into an unrecoverarble, lost state (e.g. in Vulkan backend if
we've gotten a VK_ERROR_DEVICE_LOST). If the backend context is lost, this call
will also abandon this context.
-}
isAbandoned :: (MonadIO m) => GRDirectContext -> m Bool
isAbandoned ctx = liftIO do
    fmap toBool $ gr_direct_context_is_abandoned (ptr ctx)

-- | Return the current GPU resource cache limit in bytes.
getResourceCacheLimit :: (MonadIO m) => GRDirectContext -> m Int
getResourceCacheLimit ctx = liftIO do
    fmap fromIntegral $ gr_direct_context_get_resource_cache_limit (ptr ctx)

{- | Specify the GPU resource cache limit. If the cache currently exceeds this
limit, it will be purged (LRU) to keep the cache within the limit.
-}
setResourceCacheLimit ::
    (MonadIO m) =>
    GRDirectContext ->
    -- | maxResourceBytes. The maximum number of bytes of video memory
    -- that can be held in the cache.
    Int ->
    m ()
setResourceCacheLimit ctx limit = liftIO do
    gr_direct_context_set_resource_cache_limit (ptr ctx) (fromIntegral limit)

data CacheUsage = CacheUsage
    { resourceCount :: Int
    -- ^ The number of resources that are held in the cache.
    , maxResourcesBytes :: Int
    -- ^ The total number of bytes of video memory held in the cache.
    }
    deriving (Show, Eq, Ord)

-- | Gets the current GPU resource cache usage.
getResourceCacheUsage :: (MonadIO m) => GRDirectContext -> m CacheUsage
getResourceCacheUsage ctx = evalContIO do
    resourceCount' <- useAlloca
    maxResourcesBytes' <- useAlloca
    liftIO $ gr_direct_context_get_resource_cache_usage (ptr ctx) resourceCount' maxResourcesBytes'
    resourceCount <- peekWith fromIntegral resourceCount'
    maxResourcesBytes <- peekWith fromIntegral maxResourcesBytes'
    pure CacheUsage{resourceCount, maxResourcesBytes}

-- | Frees GPU created by the context. Can be called to reduce GPU memory pressure.
freeGPUResources :: (MonadIO m) => GRDirectContext -> m ()
freeGPUResources ctx = liftIO do
    gr_direct_context_free_gpu_resources (ptr ctx)

{- | Purge GPU resources that haven't been used in the past 'msNotUsed'
milliseconds or are otherwise marked for deletion, regardless of whether the
context is under budget.
-}
performDeferredCleanup ::
    (MonadIO m) =>
    GRDirectContext ->
    -- | msNotUsed. Only unlocked resources not used in these last milliseconds
    -- will be cleaned up.
    Int ->
    m ()
performDeferredCleanup ctx delayMs = liftIO do
    gr_direct_context_perform_deferred_cleanup (ptr ctx) (fromIntegral delayMs)

{- |  Purge unlocked resources from the cache until the the provided byte count
has been reached or we have purged all unlocked resources. The default policy is
to purge in LRU order, but can be overridden to prefer purging scratch resources
(in LRU order) prior to purging other resource types.
-}
purgeUnlockedResourcesBytes ::
    (MonadIO m) =>
    GRDirectContext ->
    -- | maxBytesToPurge. the desired number of bytes to be purged.
    Int ->
    -- | preferScratchResources. If true scratch resources will be purged prior to other resource types.
    Bool ->
    m ()
purgeUnlockedResourcesBytes ctx bytesToPurge preferScratchResources = liftIO do
    gr_direct_context_purge_unlocked_resources_bytes
        (ptr ctx)
        (fromIntegral bytesToPurge)
        (fromBool preferScratchResources)

-- This entry point is intended for instances where an app has been backgrounded
-- or suspended.
--
-- If 'scratchResourcesOnly' is true all unlocked scratch resources will be
-- purged but the unlocked resources with persistent data will remain. If
-- 'scratchResourcesOnly' is false then all unlocked resources will be purged.
--
-- In either case, after the unlocked resources are purged a separate pass will
-- be made to ensure that resource usage is under budget (i.e., even if
-- 'scratchResourcesOnly' is true some resources with persistent data may be
-- purged to be under budget).
purgeUnlockedResources ::
    (MonadIO m) =>
    GRDirectContext ->
    -- | scratchResourcesOnly. If true, only unlocked scratch resources will be purged prior
    -- enforcing the budget requirements.
    Bool ->
    m ()
purgeUnlockedResources ctx scratchResourcesOnly = liftIO do
    gr_direct_context_purge_unlocked_resources (ptr ctx) (fromBool scratchResourcesOnly)

{- | Call to ensure all drawing to the context has been issued to the underlying
3D API.
-}
flush :: (MonadIO m) => GRDirectContext -> m ()
flush ctx = liftIO do
    gr_direct_context_flush (ptr ctx)

{- | Submit outstanding work to the gpu from all previously un-submitted
flushes. The return value of the submit will indicate whether or not the
submission to the GPU was successful.

If the call returns true, all previously passed in semaphores in flush calls
will have been submitted to the GPU and they can safely be waited on. The caller
should wait on those semaphores or perform some other global synchronization
before deleting the semaphores.

If it returns false, then those same semaphores will not have been submitted and
we will not try to submit them again. The caller is free to delete the
semaphores at any time.

If \"sync\" is 'True', this function will return once the gpu has finished with
all submitted work.
-}
submit ::
    (MonadIO m) =>
    GRDirectContext ->
    -- | \"sync\"
    Bool ->
    m Bool
submit ctx sync = liftIO do
    fmap toBool $ gr_direct_context_submit (ptr ctx) (fromBool sync)

{- | Call to ensure all drawing to the context has been flushed and submitted to
the underlying 3D API. This is equivalent to calling 'flush' with followed by
'submit(sync)'.
-}
flushAndSubmit ::
    (MonadIO m) =>
    GRDirectContext ->
    -- | sync
    Bool ->
    m ()
flushAndSubmit ctx sync = liftIO do
    gr_direct_context_flush_and_submit (ptr ctx) (fromBool sync)

{- | Flushes any pending uses of texture-backed images in the GPU backend.

If the image is not texture-backed (including promise texture images) or if
the 'GRDirectContext' does not have the same context ID as the context
backing the image then this is a no-op.

If the image was not used in any non-culled draws in the current queue of
work for the passed GrDirectContext then this is a no-op unless the
GrFlushInfo contains semaphores or a finish proc. Those are respected even
when the image has not been used.
-}
flushImage :: (MonadIO m) => GRDirectContext -> SKImage -> m ()
flushImage ctx image = liftIO do
    gr_direct_context_flush_image (ptr ctx) (ptr image)

{- | Issues pending SkSurface commands to the GPU-backed API objects and
resolves any SkSurface MSAA. A call to 'submit' is always required to ensure
work is actually sent to the gpu. Some specific API details:

    * GL: Commands are actually sent to the driver, but glFlush is never called. Thus some
        sync objects from the flush will not be valid until a submission occurs.

    * Vulkan/Metal/D3D/Dawn: Commands are recorded to the backend APIs corresponding command
        buffer or encoder objects. However, these objects are not sent to the gpu until a
        submission occurs.
-}
flushSurface :: (MonadIO m) => GRDirectContext -> SKSurface -> m ()
flushSurface ctx surface = liftIO do
    gr_direct_context_flush_surface (ptr ctx) (ptr surface)

type BackendState = Word32

backendState'None :: BackendState
backendState'None = 0x0

backendState'All :: BackendState
backendState'All = 0xFFFFFFFF

{- | The context normally assumes that no outsider is setting state within the
underlying 3D API's context/device/whatever. This call informs the context that
the state was modified and it should resend. Shouldn't be called frequently for
good performance.

The flag bits, \"state\", is **dependent** on which backend is used by the
context, either GL or D3D (possible in future).
-}
resetContext ::
    (MonadIO m) =>
    GRDirectContext ->
    -- \"state\"
    BackendState ->
    m ()
resetContext ctx state = liftIO do
    gr_direct_context_reset_context (ptr ctx) state

-- | Equivalent to @resetContextAll backendState'All@. See 'resetContext'.
resetContextAll :: (MonadIO m) => GRDirectContext -> m ()
resetContextAll ctx = resetContext ctx backendState'All

{- | Enumerates all cached GPU resources and dumps their memory to
traceMemoryDump.
-}
dumpMemoryStatistics ::
    (MonadIO m) =>
    GRDirectContext ->
    -- | traceMemoryDump
    SKTraceMemoryDump ->
    m ()
dumpMemoryStatistics ctx dump = liftIO do
    gr_direct_context_dump_memory_statistics (ptr ctx) (ptr dump)
