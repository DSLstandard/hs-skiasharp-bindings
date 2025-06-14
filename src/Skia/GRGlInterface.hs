module Skia.GRGlInterface (
    createNativeInterface,
    GetProcUserData,
    GetProc,
    assembleInterface,
    assembleGlInterface,
    assembleGlesInterface,
    assembleWebGlInterface,
    validate,
    hasExtension,
)
where

import Skia.Internal.Prelude

createNativeInterface :: (MonadIO m) => m (Maybe GRGlInterface)
createNativeInterface = evalContIO do
    iface' <- liftIO $ gr_glinterface_create_native_interface
    toObjectFinUnlessNull gr_glinterface_unref iface'

-- NOTE: The type should must match that of 'Gr_gl_get_proc'

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

assembleInterface :: forall a m. (MonadIO m) => FunPtr (GetProc a) -> GetProcUserData a -> m (Maybe GRGlInterface)
assembleInterface = privAssemble gr_glinterface_assemble_interface

assembleGlInterface :: forall a m. (MonadIO m) => FunPtr (GetProc a) -> GetProcUserData a -> m (Maybe GRGlInterface)
assembleGlInterface = privAssemble gr_glinterface_assemble_gl_interface

assembleGlesInterface :: forall a m. (MonadIO m) => FunPtr (GetProc a) -> GetProcUserData a -> m (Maybe GRGlInterface)
assembleGlesInterface = privAssemble gr_glinterface_assemble_gles_interface

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

hasExtension ::
    (MonadIO m) =>
    GRGlInterface ->
    -- | Extension name
    CString ->
    m Bool
hasExtension iface extName = evalContIO do
    iface' <- useObj iface
    liftIO $ fmap toBool $ gr_glinterface_has_extension iface' extName
