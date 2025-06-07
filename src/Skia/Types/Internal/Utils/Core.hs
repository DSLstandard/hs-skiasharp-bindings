module Skia.Types.Internal.Utils.Core where

import Control.Exception qualified
import Control.Monad.Cont
import Control.Monad.IO.Class
import Data.Text qualified as T
import Data.Text.Foreign qualified
import Data.Typeable
import Data.Vector.Storable qualified as VS
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Skia.Bindings
import Skia.Types.Core
import Skia.Types.Errors
import Skia.Types.Extra

toObjectMaybe :: (ManagedPtrNewType s a, SKObject s, MonadIO m) => Ptr a -> m (Maybe s)
toObjectMaybe ptr = do
    if ptr == nullPtr
        then do
            pure Nothing
        else do
            obj <- toObject ptr
            pure $ Just obj
{-# INLINE toObjectMaybe #-}

evalContIO :: (MonadIO m) => ContT a IO a -> m a
evalContIO m = liftIO $ evalContT m
{-# INLINE evalContIO #-}

useObj :: (ManagedPtrNewType s a, SKObject s) => s -> ContT r IO (Ptr a)
useObj object = ContT $ withManagedPtr (toManagedPtr object)
{-# INLINE useObj #-}

useNullIfNothing :: (a -> ContT r IO (Ptr b)) -> (Maybe a -> ContT r IO (Ptr b))
useNullIfNothing _useFunc Nothing = pure nullPtr
useNullIfNothing useFunc (Just ptr) = useFunc ptr
{-# INLINE useNullIfNothing #-}

useStorable :: (Storable s) => s -> ContT r IO (Ptr s)
useStorable s = ContT $ with s
{-# INLINE useStorable #-}

useAlloca :: forall s r. (Storable s) => ContT r IO (Ptr s)
useAlloca = ContT alloca
{-# INLINE useAlloca #-}

useAllocaArray :: forall s r. (Storable s) => Int -> ContT r IO (Ptr s)
useAllocaArray arraySize = ContT (allocaArray arraySize)
{-# INLINE useAllocaArray #-}

-- | NOTE: A CString is always null-terminateed.
useTextAsUtf8CString :: T.Text -> ContT r IO CString
useTextAsUtf8CString = ContT . Data.Text.Foreign.withCString
{-# INLINE useTextAsUtf8CString #-}

useStorableVector :: (Storable a) => VS.Vector a -> ContT r IO (Ptr a)
useStorableVector vector = ContT $ VS.unsafeWith vector

unmarshalSKEnumOrDie :: forall a s m. (Show a, Typeable s, MonadIO m, SKEnum s a) => a -> m s
unmarshalSKEnumOrDie a = liftIO do
    case unmarshalSKEnum a of
        Just s -> do
            pure s
        Nothing -> do
            let enumName = tyConName $ typeRepTyCon $ typeRep (Proxy @s)
            Control.Exception.throwIO $ UnmarshalSKEnumError $ "Unrecognized '" <> enumName <> "' enum value: " <> show a
{-# INLINE unmarshalSKEnumOrDie #-}
