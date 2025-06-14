{- | You may read <https://skia.org/docs/user/sksl/> to see how to program with
Skia's shading language (SKSL) and what uniforms and \"children\" are.
-}
module Skia.SKRuntimeEffect where

import Data.Bits
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Foreign qualified as T
import Data.Traversable
import Data.Vector qualified as Vector
import Linear
import Skia.Internal.Prelude
import Skia.SKData qualified as SKData
import Skia.SKString qualified as SKString

type ErrorLog = T.Text

-- | PRIVATE FUNCTION
privCreateForXXX ::
    (MonadIO m) =>
    -- | (SKSL source) -> (Error string destination) -> IO (Runtime effect)
    (Ptr Sk_string -> Ptr Sk_string -> IO (Ptr Sk_runtimeeffect)) ->
    -- | SKSL source
    T.Text ->
    m (Either ErrorLog SKRuntimeEffect)
privCreateForXXX createF sksl = evalContIO do
    sksl <- SKString.createFromText sksl
    sksl' <- useObj sksl

    errstr <- SKString.createEmpty
    errstr' <- useObj errstr

    effect' <- liftIO $ createF sksl' errstr'
    if effect' == nullPtr
        then do
            errtext <- SKString.getAsText errstr
            pure (Left errtext)
        else do
            Right <$> toObjectFin sk_runtimeeffect_unref effect'

createForColorFilter :: (MonadIO m) => T.Text -> m (Either ErrorLog SKRuntimeEffect)
createForColorFilter = privCreateForXXX sk_runtimeeffect_make_for_color_filter

createForShader :: (MonadIO m) => T.Text -> m (Either ErrorLog SKRuntimeEffect)
createForShader = privCreateForXXX sk_runtimeeffect_make_for_shader

createForBlender :: (MonadIO m) => T.Text -> m (Either ErrorLog SKRuntimeEffect)
createForBlender = privCreateForXXX sk_runtimeeffect_make_for_blender

-- | Flags of a 'Uniform'.
data UniformFlags = UniformFlags
    { isArray :: Bool
    -- ^ Uniform is declared as an array. 'count' contains array length.
    , isColor :: Bool
    -- ^ Uniform is declared with layout(color). Colors should be supplied as unpremultiplied,
    -- extended-range (unclamped) sRGB (ie SkColor4f). The uniform will be automatically
    -- transformed to unpremultiplied extended-range working-space colors.
    , isVertex :: Bool
    -- ^ When used with SkMeshSpecification, indicates that the uniform is
    -- present in the vertex shader. Not used with SkRuntimeEffect.
    , isFragment :: Bool
    -- ^ When used with SkMeshSpecification, indicates that the uniform is
    -- present in the fragment shader. Not used with SkRuntimeEffect.
    , isHalfPrecision :: Bool
    -- ^ This flag indicates that the SkSL uniform uses a medium-precision type
    -- (i.e., `half` instead of `float`).
    }
    deriving (Show, Eq, Ord)

-- | Reflected description of a uniform variable in the effect's SkSL
data Uniform = Uniform
    { name :: T.Text
    , offset :: Int
    , type_ :: SKRuntimeEffectUniformType
    , count :: Int
    , flags :: UniformFlags
    }
    deriving (Show, Eq, Ord)

unmarshalUniform :: (MonadIO m) => Sk_runtimeeffect_uniform -> m Uniform
unmarshalUniform raw = liftIO do
    name <- liftIO $ T.peekCStringLen (raw.fName, fromIntegral raw.fNameLength)
    type_ <- unmarshalSKEnumOrDie raw.fType
    pure
        Uniform
            { name = name
            , offset = fromIntegral raw.fOffset
            , type_ = type_
            , count = fromIntegral raw.fCount
            , flags = unmarshalUniformFlags raw.fFlags
            }
  where
    unmarshalUniformFlags :: Sk_runtimeeffect_uniform_flags -> UniformFlags
    unmarshalUniformFlags input =
        -- See include/effects/SkRuntimeEffect.h's struct RuntimeEffect's struct Uniform's enum Flags.
        UniformFlags
            { isArray = testBit input 0
            , isColor = testBit input 1
            , isVertex = testBit input 2
            , isFragment = testBit input 3
            , isHalfPrecision = testBit input 4
            }

-- | Returns the number of uniforms in the 'SKRuntimeEffect'.
getUniformCount :: (MonadIO m) => SKRuntimeEffect -> m Int
getUniformCount effect = evalContIO do
    effect' <- useObj effect
    liftIO $ fmap fromIntegral $ sk_runtimeeffect_get_uniforms_size effect'

{- | Returns the uniform at an index. The index must be within
[0..'getUniformCount'), or else 'Nothing' is returned.
-}
getUniformByIndex :: (MonadIO m) => SKRuntimeEffect -> Int -> m (Maybe Uniform)
getUniformByIndex effect i = evalContIO do
    count <- getUniformCount effect
    if 0 <= i && i < count
        then do
            effect' <- useObj effect
            uniform' <- useAlloca
            liftIO $ sk_runtimeeffect_get_uniform_from_index effect' (fromIntegral i) uniform'
            uniform <- unmarshalUniform =<< peekWith id uniform'
            pure $ Just uniform
        else do
            pure Nothing

{- | Returns all uniforms from index 0 to 'getUniformCount'-1.

Note that using this function is faster than iterating over 'getUniformByIndex'
to generate the result.
-}
getAllUniforms :: (MonadIO m) => SKRuntimeEffect -> m (Vector.Vector Uniform)
getAllUniforms effect = evalContIO do
    effect' <- useObj effect
    uniform' <- useAlloca

    count <- getUniformCount effect
    Vector.generateM count \i -> do
        liftIO $ sk_runtimeeffect_get_uniform_from_index effect' (fromIntegral i) uniform'
        unmarshalUniform =<< peekWith id uniform'

{- | Returns the combined byte sizes of all 'uniform' variables. When calling
'makeColorFilter' or 'makeShader', provide an 'BS.ByteString' of this byte size,
containing values for all of those variables.
-}
getTotalUniformByteSize :: (MonadIO m) => SKRuntimeEffect -> m Int
getTotalUniformByteSize effect = evalContIO do
    effect' <- useObj effect
    bytesz <- liftIO $ sk_runtimeeffect_get_uniform_byte_size effect'
    pure $ fromIntegral bytesz

data Child = Child
    { name :: T.Text
    , type_ :: SKRuntimeEffectChildType
    , index :: Int
    }
    deriving (Show, Eq, Ord)

unmarshalChild :: (MonadIO m) => Sk_runtimeeffect_child -> m Child
unmarshalChild raw = evalContIO do
    name <- liftIO $ T.peekCStringLen (raw.fName, fromIntegral raw.fNameLength)
    type_ <- unmarshalSKEnumOrDie raw.fType
    pure
        Child
            { name
            , type_
            , index = fromIntegral raw.fIndex
            }

-- | Returns the number of children in the 'SKRuntimeEffect'.
getChildCount :: (MonadIO m) => SKRuntimeEffect -> m Int
getChildCount effect = evalContIO do
    effect' <- useObj effect
    liftIO $ fmap fromIntegral $ sk_runtimeeffect_get_children_size effect'

{- | Returns the child at an index. The index must be within
[0..'getChildCount'), or else 'Nothing' is returned.
-}
getChildByIndex :: (MonadIO m) => SKRuntimeEffect -> Int -> m (Maybe Child)
getChildByIndex effect i = evalContIO do
    count <- getChildCount effect
    if 0 <= i && i < count
        then do
            effect' <- useObj effect
            child' <- useAlloca
            liftIO $ sk_runtimeeffect_get_child_from_index effect' (fromIntegral i) child'
            child <- unmarshalChild =<< peekWith id child'
            pure $ Just child
        else do
            pure Nothing

{- | Returns all uniforms from index 0 to 'getChildCount'-1.

Note that using this function is faster than iterating over 'getChildByIndex' to
generate the result.
-}
getAllChildren :: (MonadIO m) => SKRuntimeEffect -> m (Vector.Vector Child)
getAllChildren effect = evalContIO do
    effect' <- useObj effect
    child' <- useAlloca

    count <- getChildCount effect
    Vector.generateM count \i -> do
        liftIO $ sk_runtimeeffect_get_child_from_index effect' (fromIntegral i) child'
        unmarshalChild =<< peekWith id child'

data EffectChild
    = EffectChild'Shader SKShader
    | EffectChild'ColorFilter SKColorFilter
    | EffectChild'Blender SKBlender
    deriving (Show)

castEffectChildToFlattenable :: EffectChild -> SKFlattenable
castEffectChildToFlattenable = \case
    EffectChild'Shader shader -> toA SKFlattenable shader
    EffectChild'ColorFilter cf -> toA SKFlattenable cf
    EffectChild'Blender blender -> toA SKFlattenable blender

{- | Creates an 'SKShader' from an 'SKRuntimeEffect' program with the specified
inputs. Returns 'Nothing' if the operation fails (e.g., due to incorrect input
uniform data byte size, the 'SKRuntimeEffect' prohibiting the creation of
'SKShader's, etc).
-}
makeShader ::
    (MonadIO m) =>
    SKRuntimeEffect ->
    -- | Uniform data input.
    --
    -- This is a single 'BS.ByteString' of length 'getTotalUniformByteSize'
    -- packing all the uniform values in accordance to the byte offsets and
    -- sizes of each 'Uniform' in the list returned by 'getAllUniforms'.
    --
    -- To build a 'BS.ByteString', consider using
    -- <https://hackage-content.haskell.org/package/bytestring/docs/Data-ByteString-Builder.html>
    -- or <https://hackage.haskell.org/package/binary/docs/Data-Binary-Put.html>.
    BS.ByteString ->
    -- | Children input.
    [EffectChild] ->
    -- | Optional local matrix
    Maybe (M33 Float) ->
    m (Maybe (Ref SKShader))
makeShader effect inUniformData inChildren localMatrix = evalContIO do
    -- TODO: Provide utils to build uniformData

    -- TODO: Must inChildren's length have to equal 'getChildCount'?

    effect' <- useObj effect

    children :: [Ptr Sk_flattenable] <- for inChildren \inChild -> do
        useObj (castEffectChildToFlattenable inChild)
    (children', childrenCount) <- ContT $ withArrayLen' children

    uniforms <- SKData.createFromByteString inUniformData
    uniforms' <- useObj uniforms

    localMatrix' <- useNullIfNothing useStorable $ fmap toSKMatrix localMatrix

    shader' <-
        liftIO $
            sk_runtimeeffect_make_shader
                effect'
                uniforms'
                children'
                (fromIntegral childrenCount)
                localMatrix'
    toObjectFinUnlessNull sk_shader_unref shader'
