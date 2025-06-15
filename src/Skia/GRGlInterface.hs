module Skia.GRGlInterface (
    -- * Creating 'GRGlInterface'
    createNativeInterface,
    GetProcUserData,
    GetProc,
    assembleInterface,
    assembleGlInterface,
    assembleGlesInterface,
    assembleWebGlInterface,

    -- * Miscellaneous utilities.
    validate,
    hasExtension,
)
where

import Control.Exception
import Skia.Internal.Prelude

{- | Creates a native 'GRGlInterface'. Depending the execution platform, a GL
interface (e.g., GLX, WGL, EGL, etc) will be picked. If no 'GRGlInterface' is
constructed, an 'SkiaError' is thrown.
-}
createNativeInterface :: Acquire GRGlInterface
createNativeInterface =
    mkSKObjectAcquire
        ( do
            iface' <- gr_glinterface_create_native_interface
            when (iface' == nullPtr) do
                throwIO $ SkiaError "Cannot create native GL interface"
            pure iface'
        )
        gr_glinterface_unref

-- | See documentation of 'GetProc'
type GetProcUserData a = Ptr a

{- | A type alias for 'Gr_gl_get_proc'.

This is a type for functions that resolve OpenGL function names to real function
addresses.
-}
type GetProc a =
    -- | A pointer to some user data. You may use it however you like to help in
    -- resolving OpenGL functions.
    GetProcUserData a ->
    -- | Input OpenGL function name.
    CString ->
    -- | The return value should be the corresponding OpenGL function.
    IO (FunPtr Gr_gl_func_ptr)

privAssemble ::
    (MonadIO m) =>
    (Ptr () -> FunPtr Gr_gl_get_proc -> IO (Ptr Gr_glinterface)) ->
    (FunPtr (GetProc a) -> GetProcUserData a -> m (Maybe GRGlInterface))
privAssemble assembleFn getprocfunptr userData = liftIO do
    iface' <- assembleFn (castPtr userData) (castFunPtr getprocfunptr)
    toObjectFinUnlessNull gr_glinterface_unref iface'

{- | Generic function for creating a GrGLInterface for an either OpenGL or GLES.
It calls the get proc function to get each function address.
-}
assembleInterface :: forall a m. (MonadIO m) => FunPtr (GetProc a) -> GetProcUserData a -> m (Maybe GRGlInterface)
assembleInterface = privAssemble gr_glinterface_assemble_interface

{- | Generic function for creating a GrGLInterface for an OpenGL (but not GLES)
context. It calls the input get proc function to get each function address.
-}
assembleGlInterface :: forall a m. (MonadIO m) => FunPtr (GetProc a) -> GetProcUserData a -> m (Maybe GRGlInterface)
assembleGlInterface = privAssemble gr_glinterface_assemble_gl_interface

{- | Generic function for creating a GrGLInterface for an OpenGL ES (but not
Open GL) context. It calls the input get proc function to get each function
address.
-}
assembleGlesInterface :: forall a m. (MonadIO m) => FunPtr (GetProc a) -> GetProcUserData a -> m (Maybe GRGlInterface)
assembleGlesInterface = privAssemble gr_glinterface_assemble_gles_interface

{- | Generic function for creating a GrGLInterface for a WebGL (similar to
OpenGL ES) context. It calls the input get proc function to get each function
address.
-}
assembleWebGlInterface :: forall a m. (MonadIO m) => FunPtr (GetProc a) -> GetProcUserData a -> m (Maybe GRGlInterface)
assembleWebGlInterface = privAssemble gr_glinterface_assemble_webgl_interface

{- | Validates that the 'GRGlInterface' supports its advertised standard. This
means the necessary function pointers have been initialized for both the GL
version and any advertised extensions.
-}
validate :: (MonadIO m) => GRGlInterface -> m Bool
validate iface = evalContIO do
    iface' <- useObj iface
    liftIO $ fmap toBool $ gr_glinterface_validate iface'

{- | This helper function queries the current GL context for its extensions and
remembers them (internally done by Google Skia). It supports both @glGetString-@
and @glGetStringi-@style extension string APIs and will use the latter if it is
available. It also will query for EGL extensions if a @eglQueryString@
implementation is provided.
-}
hasExtension ::
    (MonadIO m) =>
    GRGlInterface ->
    -- | OpenGL extension name
    CString ->
    m Bool
hasExtension iface extName = evalContIO do
    iface' <- useObj iface
    liftIO $ fmap toBool $ gr_glinterface_has_extension iface' extName
