{- | This module deals with 'SKRefCnt' objects.

The functions in this module are not meant to be used by typical users of
this library - the APIs of this library is designed in such a way that
end-users do not have to care about the boring low-level reference counting
details. However, if you wish, you may use the functions here to perform
unsafe operations.
-}
module Skia.SKRefCnt where

import Skia.Internal.Prelude

{- | Increments the reference counter of the input 'SKObject'. Decrements when
'Acquire' is released.

This effectively keeps the input 'SKObject' alive at least unless this 'Acquire'
releases.
-}
holdReferenceCount :: (SKObject s, IsSKRefCnt s) => s -> Acquire ()
holdReferenceCount (toA SKRefCnt -> refcnt) =
    mkAcquire
        (sk_refcnt_safe_ref (ptr refcnt))
        (\() -> sk_refcnt_safe_unref (ptr refcnt))

-- | Increments the reference counter of the input 'SKObject'.
increment :: (MonadIO m, SKObject s, IsSKRefCnt s) => s -> m ()
increment (toA SKRefCnt -> refcnt) = liftIO do
    sk_refcnt_safe_ref (ptr refcnt)

-- | Decrements the reference counter of the input 'SKObject'.
decrement :: (MonadIO m, SKObject s, IsSKRefCnt s) => s -> m ()
decrement (toA SKRefCnt -> refcnt) = liftIO do
    sk_refcnt_safe_unref (ptr refcnt)

-- | Returns the reference count of the input 'SKObject'.
getRefCount :: (MonadIO m, SKObject s, IsSKRefCnt s) => s -> m Int
getRefCount (toA SKRefCnt -> refcnt) = liftIO do
    fromIntegral <$> sk_refcnt_get_ref_count (ptr refcnt)

-- | 'SKNVRefCnt' version of 'holdReferenceCount'.
holdReferenceCountNV :: (SKObject s, IsSKNVRefCnt s) => s -> Acquire ()
holdReferenceCountNV (toA SKNVRefCnt -> refcnt) =
    mkAcquire
        (sk_nvrefcnt_safe_ref (ptr refcnt))
        (\() -> sk_nvrefcnt_safe_unref (ptr refcnt))

-- | 'SKNVRefCnt' version of 'increment'.
incrementNV :: (MonadIO m, SKObject s, IsSKNVRefCnt s) => s -> m ()
incrementNV (toA SKNVRefCnt -> refcnt) = liftIO do
    sk_nvrefcnt_safe_ref (ptr refcnt)

-- | 'SKNVRefCnt' version of 'decrement'.
decrementNV :: (MonadIO m, SKObject s, IsSKNVRefCnt s) => s -> m ()
decrementNV (toA SKNVRefCnt -> refcnt) = liftIO do
    sk_nvrefcnt_safe_unref (ptr refcnt)

-- | 'SKNVRefCnt' version of 'getRefCount'.
getRefCountNV :: (MonadIO m, SKObject s, IsSKNVRefCnt s) => s -> m Int
getRefCountNV (toA SKNVRefCnt -> refcnt) = liftIO do
    fromIntegral <$> sk_nvrefcnt_get_ref_count (ptr refcnt)
