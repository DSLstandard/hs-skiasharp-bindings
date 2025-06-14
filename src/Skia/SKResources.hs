{- | TODO: This module does not have enough functionalities.

Many functions in Mono Skia is not included here because they are incomplete.
-}
module Skia.SKResources where

import Foreign.C.String qualified
import Skia.Internal.Prelude
import Skia.SKString qualified as SKString

-- | Convenience type alias to 'SKResourcesResourceProvider'.
type ResourceProvider = SKResourcesResourceProvider

createFileResourceProvider ::
    (MonadIO m) =>
    -- | Base directory
    FilePath ->
    -- | Predecode?
    --
    -- For reference, Google Skia puts 'False' as the default.
    Bool ->
    m (Ref ResourceProvider)
createFileResourceProvider baseDir predecode = evalContIO do
    baseDir <- SKString.createFromString baseDir
    baseDir' <- useObj baseDir

    rp' <- liftIO $ skresources_file_resource_provider_make baseDir' (fromBool predecode)
    toObjectFin skresources_resource_provider_unref rp'

createCachingResourceProviderProxy ::
    (MonadIO m) =>
    ResourceProvider ->
    m (Ref ResourceProvider)
createCachingResourceProviderProxy rp = evalContIO do
    rp' <- useObj rp
    newRp' <- liftIO $ skresources_caching_resource_provider_proxy_make rp'
    toObjectFin skresources_resource_provider_unref newRp'

createDataUriResourceProviderProxy ::
    (MonadIO m) =>
    ResourceProvider ->
    -- | Predecode?
    --
    -- For reference, Google Skia puts 'False' as the default.
    Bool ->
    m (Ref ResourceProvider)
createDataUriResourceProviderProxy rp predecode = evalContIO do
    rp' <- useObj rp
    newRp' <- liftIO $ skresources_data_uri_resource_provider_proxy_make rp' (fromBool predecode)
    toObjectFin skresources_resource_provider_unref newRp'

{- | Load a generic resource (currently only nested animations) specified by
@'path' + 'name'@, and return as an 'SKData'.

Returns 'Nothing' if operation fails.
-}
load ::
    (MonadIO m) =>
    SKResourcesResourceProvider ->
    -- | Resource path
    FilePath ->
    -- | Resource name
    String ->
    m (Maybe (Ref SKData))
load provider path name = evalContIO do
    provider' <- useObj provider
    path' <- ContT $ Foreign.C.String.withCString path
    name' <- ContT $ Foreign.C.String.withCString name
    dat' <- liftIO $ skresources_resource_provider_load provider' path' name'
    toObjectFinUnlessNull sk_data_unref dat'
