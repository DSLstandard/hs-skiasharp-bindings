{-# LANGUAGE CPP #-}

module Skia.Types.Core where

#ifdef HS_SKIA_CHECK_NULLPTR_ENABLED
import qualified Control.Exception
import Skia.Types.Errors
#endif

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Foreign
import GHC.Base
import GHC.ForeignPtr
import GHC.TypeError
import Skia.Internal.Bool qualified as TB
import Data.Typeable
import System.IO.Unsafe qualified

newtype FinalizationDebugTracer = FinalizationDebugTracer
    { runFinalizationDebugTracer :: forall a. Typeable a => Ptr a -> IO ()
    }

-- | This is 'True' if the package flag @enable-debug-trace-finalizers@ is
-- enabled, 'False' otherwise.
isFinalizationDebugTracerFlagEnabled :: Bool
isFinalizationDebugTracerFlagEnabled =
#ifdef HS_SKIA_DEBUG_TRACE_FINALIZERS_ENABLED
    True
#else
    False
#endif
{-# INLINE isFinalizationDebugTracerFlagEnabled #-}

globalFinalizationDebugTracerRef :: Data.IORef.IORef (Maybe FinalizationDebugTracer)
globalFinalizationDebugTracerRef =
    if isFinalizationDebugTracerFlagEnabled
        then System.IO.Unsafe.unsafePerformIO $ newIORef Nothing
        else error "FinalizationDebugTracer is not supported"
{-# NOINLINE globalFinalizationDebugTracerRef #-}

-- | Set the global finalization debug tracer.
--
-- NOTE: The package flag @enable-debug-trace-finalizers@ must be enabled.
setFinalizationDebugTracer :: MonadIO m => Maybe FinalizationDebugTracer -> m ()
setFinalizationDebugTracer tracer = liftIO do
    if isFinalizationDebugTracerFlagEnabled
        then Data.IORef.writeIORef globalFinalizationDebugTracerRef tracer
        else error "FinalizationDebugTracer is not supported"

-- | Returns the current global finalization debug tracer.
--
-- The global finalization debug tracer is 'Nothing' initially.
--
-- NOTE: Package flag @enable-debug-trace-finalizers@ must be enabled.
getFinalizationDebugTracer :: MonadIO m => m (Maybe FinalizationDebugTracer)
getFinalizationDebugTracer = liftIO do
    if isFinalizationDebugTracerFlagEnabled
        then Data.IORef.readIORef globalFinalizationDebugTracerRef
        else error "FinalizationDebugTracer is not supported"

--
-- ### ManagedPtrs
--
-- Reference implementation:
-- https://hackage.haskell.org/package/haskell-gi-base-0.26.8/docs/Data-GI-Base-ManagedPtr.html
--

data ManagedPtr a = ManagedPtr
    { foreignPtr :: ForeignPtr a
    , isDisowned :: IORef Bool
    }

-- The Show instance implementation is pretty non-standard-compliant, but this
-- helps with debugging.
instance Show (ManagedPtr a) where
    show ManagedPtr{..} = show foreignPtr

newManagedPtr ::
    (MonadIO m) =>
    Ptr a ->
    -- | Finalizer
    IO () ->
    m (ManagedPtr a)
newManagedPtr ptr finalizerFunc = liftIO do
    isDisowned <- newIORef False
    foreignPtr <- newConcForeignPtr ptr do
        disowned <- readIORef isDisowned
        unless disowned finalizerFunc
    pure ManagedPtr{..}

newManagedPtr_ ::
    (MonadIO m) =>
    Ptr a ->
    m (ManagedPtr a)
newManagedPtr_ ptr = liftIO do
    isDisowned <- newIORef False
    foreignPtr <- newForeignPtr_ ptr
    pure ManagedPtr{..}

finalizeManagedPtr :: (MonadIO m) => ManagedPtr a -> m ()
finalizeManagedPtr manptr = liftIO do
    finalizeForeignPtr manptr.foreignPtr

disownManagedPtr :: (MonadIO m) => ManagedPtr a -> m (Ptr a)
disownManagedPtr manptr = liftIO do
    withForeignPtr manptr.foreignPtr \_ -> do
        atomicWriteIORef manptr.isDisowned True
        finalizeForeignPtr manptr.foreignPtr
        pure $ unsafeForeignPtrToPtr manptr.foreignPtr

castManagedPtr :: ManagedPtr a -> ManagedPtr b
castManagedPtr ManagedPtr{..} =
    ManagedPtr
        { isDisowned
        , foreignPtr = castForeignPtr foreignPtr
        }

withManagedPtr :: ManagedPtr a -> (Ptr a -> IO r) -> IO r
withManagedPtr manptr = withForeignPtr manptr.foreignPtr

class ManagedPtrNewType s a | s -> a where
    fromManagedPtr :: ManagedPtr a -> s

    toManagedPtr :: s -> ManagedPtr a

class SKEnum s a | s -> a where
    marshalSKEnum :: s -> a
    unmarshalSKEnum :: a -> Maybe s

-- | NOTE: The 'Typeable' is here to help with certain debugging capabilities.
class Typeable s => SKObject s where
    fromAnyManagedPtr :: ManagedPtr xxx -> s
    toAnyManagedPtr :: s -> ManagedPtr xxx

unsafeCastObject :: (SKObject a, SKObject b) => a -> b
unsafeCastObject = fromAnyManagedPtr . toAnyManagedPtr @_ @()

toObject :: (ManagedPtrNewType s a, SKObject s, MonadIO m) => Ptr a -> m s
toObject ptr = liftIO do
#ifdef HS_SKIA_CHECK_NULLPTR_ENABLED
    when (ptr == nullPtr) do
        Control.Exception.throwIO $ InternalError "toObject encountered nullptr"
#endif
    fptr <- newManagedPtr_ ptr
    pure $ fromManagedPtr fptr

toObjectFin :: forall s a m.
    ( ManagedPtrNewType s a
    , SKObject s
    , MonadIO m
    , Typeable a
    ) =>
    (Ptr a -> IO ()) -> Ptr a -> m s
toObjectFin finalizer ptr = liftIO do
#ifdef HS_SKIA_CHECK_NULLPTR_ENABLED
    when (ptr == nullPtr) do
        Control.Exception.throwIO $ InternalError "toObjectFin encountered nullptr"
#endif
    fptr <- newManagedPtr ptr do
#ifdef HS_SKIA_DEBUG_TRACE_FINALIZERS_ENABLED
        tracer <- getFinalizationDebugTracer
        case tracer of
            Nothing -> do
                pure () -- Do nothing
            Just tracer -> do
                runFinalizationDebugTracer tracer ptr
#endif
        finalizer ptr
    pure $ fromManagedPtr fptr
           
toObjectFinUnlessNull :: (ManagedPtrNewType s a, SKObject s, MonadIO m, Typeable a) => (Ptr a -> IO ()) -> Ptr a -> m (Maybe s)
toObjectFinUnlessNull finalizer ptr = liftIO do
    if ptr == nullPtr
        then pure Nothing
        else Just <$> toObjectFin finalizer ptr

disposeObject :: (SKObject s, MonadIO m) => s -> m ()
disposeObject object = liftIO do
    finalizeManagedPtr $ toAnyManagedPtr object

disownObject :: (ManagedPtrNewType s a, SKObject s, MonadIO m) => s -> m (Ptr a)
disownObject s = liftIO do
    let a = toManagedPtr s
    disownManagedPtr a

--
-- ### OOP inheritance
--
-- Implementation is adapted from
--  https://hackage.haskell.org/package/haskell-gi-base-0.26.8/docs/Data-GI-Base-Overloading.html.
--
-- Modifications are made to 1) allow transitivity in ancestor checks and 2)
-- improve error messages.
--

{- | All the types that are ascendants of this type, including
interfaces that the type implements.
-}
type family ParentTypes a :: [Type]

{- | Helper type for 'IsDescendantOf'.

Recursively (in the sense that the ParentTypes of parents are also checked)
check if 'parent' is contained in 'parents'.
-}
type family ExistsParent (parent :: Type) (parents :: [Type]) :: Type where
    ExistsParent parent '[] =
        TB.False
    ExistsParent parent (parent ': parents) =
        TB.True
    ExistsParent parent (parent__ ': parents) =
        TB.Or
            (ExistsParent parent (ParentTypes parent__))
            (ExistsParent parent parents)

{- | Check that a type is in the list of `ParentTypes` of another
type.
-}
type family IsDescendantOf (parent :: Type) (t :: Type) :: Type where
    -- Every object is defined to be a descendant of itself.
    IsDescendantOf t t = TB.True
    IsDescendantOf parent t = ExistsParent parent (ParentTypes t)

-- | Used by 'IsSubclassOf' to signal an error.
type TypeError'NotSubclassOf super sub =
    TypeError
        ( 'Text "‘"
            ':<>: 'ShowType sub
            ':<>: 'Text "’ is not a subclass of ‘"
            ':<>: 'ShowType super
            ':<>: 'Text "’"
        )

type IsSubclassOf super sub =
    ( SKObject super
    , SKObject sub
    , {-
          Explanation:

          when (IsDescendantOf super sub) is True, this becomes () ~ () and the typechecker is satisfied;

          when (IsDescendantOf super sub) is False, this becomes (TypeError'NotSubclassOf super sub) and the typechecker reports an error.
      -}
      TB.If (IsDescendantOf super sub) () (TypeError'NotSubclassOf super sub) ~ ()
    )

asA ::
    (IsSubclassOf super sub) =>
    sub ->
    (ManagedPtr super' -> super) ->
    super
asA sub _constructor = unsafeCastObject sub

toA ::
    (IsSubclassOf super sub) =>
    (ManagedPtr super' -> super) ->
    sub ->
    super
toA = flip asA

pointerCast ::
    ( IsSubclassOf super sub
    , ManagedPtrNewType super super'
    , ManagedPtrNewType sub sub'
    ) =>
    (ManagedPtr sub' -> sub) ->
    (ManagedPtr super' -> super) ->
    Ptr sub' ->
    Ptr super'
pointerCast _sub _super = castPtr
