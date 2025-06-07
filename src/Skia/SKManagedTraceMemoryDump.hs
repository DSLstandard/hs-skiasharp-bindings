module Skia.SKManagedTraceMemoryDump where

import Skia.Internal.Prelude

data TraceMemoryDumpParams = TraceMemoryDumpParams
    { dumpNumericValue :: CString -> CString -> CString -> Word64 -> IO ()
    -- ^ (Dump name) -> (Value name) -> (Units) -> (uint64 Value) -> IO ()
    , dumpStringValue :: CString -> CString -> CString -> IO ()
    -- ^ (Dump name) -> (Value name) -> -> (String Value) -> IO ()
    }

initializeProcs :: (MonadIO m) => m ()
initializeProcs = liftIO do
    fDumpNumericValue <- mkFunPtr'Sk_managedtraceMemoryDump_dumpNumericValue_proc \_ ctx dumpName valueName units value -> do
        params <- interpretCtx ctx
        params.dumpNumericValue dumpName valueName units value

    fDumpStringValue <- mkFunPtr'Sk_managedtraceMemoryDump_dumpStringValue_proc \_ ctx dumpName valueName value -> do
        params <- interpretCtx ctx
        params.dumpStringValue dumpName valueName value

    sk_managedtracememorydump_set_procs Sk_managedtracememorydump_procs{..}
  where
    interpretCtx :: Ptr () -> IO TraceMemoryDumpParams
    interpretCtx ptr = deRefStablePtr (castPtrToStablePtr ptr)

newtype DestroyHandle = DestroyHandle
    { runDestroyHandle :: IO ()
    }

destroy :: (MonadIO m) => DestroyHandle -> m ()
destroy = liftIO . runDestroyHandle

createFromParams ::
    (MonadIO m) =>
    -- | Detailed?
    Bool ->
    -- | Dump wrapped?
    Bool ->
    TraceMemoryDumpParams ->
    m (DestroyHandle, SKManagedTraceMemoryDump)
createFromParams detailed dumpWrapped params = liftIO do
    params' <- newStablePtr params

    dump' <-
        sk_managedtracememorydump_new
            (fromBool detailed)
            (fromBool dumpWrapped)
            (castStablePtrToPtr params')
    dump <- toObject dump'

    let
        destroyHandle :: DestroyHandle
        destroyHandle = DestroyHandle do
            sk_managedtracememorydump_delete dump'
            freeStablePtr params'

    pure (destroyHandle, dump)
