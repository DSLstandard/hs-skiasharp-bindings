module Skia.Types.Core where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Foreign
import GHC.Base
import GHC.ForeignPtr
import GHC.TypeError

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

class SKObject s where
    fromAnyManagedPtr :: ManagedPtr xxx -> s
    toAnyManagedPtr :: s -> ManagedPtr xxx

unsafeCastObject :: (SKObject a, SKObject b) => a -> b
unsafeCastObject = fromAnyManagedPtr . toAnyManagedPtr @_ @()

toObject :: (ManagedPtrNewType s a, SKObject s, MonadIO m) => Ptr a -> m s
toObject ptr = liftIO do
    fptr <- newManagedPtr_ ptr
    pure $ fromManagedPtr fptr

toObjectFin :: (ManagedPtrNewType s a, SKObject s, MonadIO m) => (Ptr a -> IO ()) -> Ptr a -> m s
toObjectFin finalizer ptr = liftIO do
    fptr <- newManagedPtr ptr (finalizer ptr)
    pure $ fromManagedPtr fptr

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

{- | Look in the given list of (symbol, tag) tuples for the tag
corresponding to the given symbol. If not found raise the given
type error.
-}
type family
    FindElement
        (m :: Symbol)
        (ms :: [(Symbol, Type)])
        (typeError :: ErrorMessage) ::
        Type
    where
    FindElement m '[] typeError = TypeError typeError
    FindElement m ('(m, o) ': ms) typeError = o
    FindElement m ('(m', o) ': ms) typeError = FindElement m ms typeError

{- | All the types that are ascendants of this type, including
interfaces that the type implements.
-}
type family ParentTypes a :: [Type]

{- | A constraint on a type, to be fulfilled whenever it has a type
instance for `ParentTypes`. This leads to nicer errors, thanks to
the overlappable instance below.
-}
class HasParentTypes (o :: Type)

{- | Default instance, which will give rise to an error for types
without an associated `ParentTypes` instance.
-}
instance
    {-# OVERLAPPABLE #-}
    ( TypeError
        ( 'Text "Type ‘"
            ':<>: 'ShowType a
            ':<>: 'Text "’ does not have any known parent types."
        )
    ) =>
    HasParentTypes a

{- | Check whether a type appears in a list. We specialize the
names/types a bit so the error messages are more informative.
-}
type family CheckForAncestorType t (a :: Type) (as :: [Type]) :: Constraint where
    CheckForAncestorType t a '[] =
        TypeError
            ( 'Text "Required ancestor ‘"
                ':<>: 'ShowType a
                ':<>: 'Text "’ not found for type ‘"
                ':<>: 'ShowType t
                ':<>: 'Text "’."
            )
    CheckForAncestorType t a (a ': as) = ()
    CheckForAncestorType t a (b ': as) = CheckForAncestorType t a as

{- | Check that a type is in the list of `ParentTypes` of another
type.
-}
type family IsDescendantOf (parent :: Type) (descendant :: Type) :: Constraint where
    -- Every object is defined to be a descendant of itself.
    IsDescendantOf d d = ()
    IsDescendantOf p d = CheckForAncestorType d p (ParentTypes d)

type IsSubclassOf super sub =
    ( SKObject super
    , SKObject sub
    , HasParentTypes sub
    , IsDescendantOf super sub
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
