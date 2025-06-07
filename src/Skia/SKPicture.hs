module Skia.SKPicture where

import Linear
import Skia.Internal.Prelude

getUniqueId :: (MonadIO m) => SKPicture -> m Word32
getUniqueId picture = evalContIO do
    picture' <- useObj picture
    liftIO $ fmap coerce $ sk_picture_get_unique_id picture'

getCullRect :: (MonadIO m) => SKPicture -> m (Rect Float)
getCullRect picture = evalContIO do
    picture' <- useObj picture
    rect' <- useAlloca
    liftIO $ sk_picture_get_cull_rect picture' rect'
    peekWith fromSKRect rect'

makeShader ::
    (MonadIO m) =>
    SKPicture ->
    -- | X Y tile mode
    V2 SKShaderTileMode ->
    SKFilterMode ->
    -- | Optional local matrix
    Maybe (M33 Float) ->
    -- | The tile rectangle in picture coordinates: this represents the subset (or superset) of the picture used when building a tile. It is not affected by localMatrix and does not imply scaling (only translation and cropping). If 'Nothing', the tile rect is considered equal to the picture bounds.
    Maybe (Rect Float) ->
    m SKShader
makeShader picture (V2 tx ty) filterMode localMatrix tileRect = evalContIO do
    picture' <- useObj picture
    localMatrix' <- useNullIfNothing useStorable $ fmap toSKMatrix $ localMatrix
    tileRect' <- useNullIfNothing useStorable $ fmap toSKRect $ tileRect
    shader' <-
        liftIO $
            sk_picture_make_shader
                picture'
                (marshalSKEnum tx)
                (marshalSKEnum ty)
                (marshalSKEnum filterMode)
                localMatrix'
                tileRect'
    toObjectFin sk_shader_unref shader'

serializeToData :: (MonadIO m) => SKPicture -> m SKData
serializeToData picture = evalContIO do
    picture' <- useObj picture
    dat' <- liftIO $ sk_picture_serialize_to_data picture'
    toObjectFin sk_data_unref dat'

serializeToStream :: (MonadIO m, IsSKWStream stream) => SKPicture -> stream -> m ()
serializeToStream picture (toA SKWStream -> stream) = evalContIO do
    picture' <- useObj picture
    stream' <- useObj stream
    liftIO $ sk_picture_serialize_to_stream picture' stream'

createFromStream ::
    (MonadIO m, IsSKStream stream) =>
    stream ->
    -- | Returns 'Nothing' when the operation fails. Operation fails if data
    -- does not permit constructing valid 'SKPicture'.
    m (Maybe SKPicture)
createFromStream (toA SKStream -> stream) = evalContIO do
    stream' <- useObj stream
    picture' <- liftIO $ sk_picture_deserialize_from_stream stream'
    if picture' == nullPtr
        then pure Nothing
        else Just <$> toObjectFin sk_picture_unref picture'

createFromData ::
    (MonadIO m, IsSKStream stream) =>
    SKData ->
    -- | Returns 'Nothing' when the operation fails. Operation fails if data
    -- does not permit constructing valid 'SKPicture'.
    m (Maybe SKPicture)
createFromData dat = evalContIO do
    dat' <- useObj dat
    picture' <- liftIO $ sk_picture_deserialize_from_data dat'
    if picture' == nullPtr
        then pure Nothing
        else Just <$> toObjectFin sk_picture_unref picture'

{- | Replays the drawing commands on the specified canvas. In the case that the
commands are recorded, each command in the SkPicture is sent separately to
canvas.

To add a single command to draw SkPicture to recording canvas, call SKCanvas's
@drawPicture@ instead.
-}
playback ::
    (MonadIO m, IsSKCanvas canvas) =>
    SKPicture ->
    -- | Destination canvas
    canvas ->
    m ()
playback picture (toA SKCanvas -> canvas) = evalContIO do
    picture' <- useObj picture
    canvas' <- useObj canvas
    liftIO $ sk_picture_playback picture' canvas'

{- | Returns the approximate number of operations in 'SKPicture'. Returned value
may be greater or less than the number of 'SKCanvas' calls recorded: some calls
may be recorded as more than one operation, other calls may be optimized away.
-}
approximateOpCount ::
    (MonadIO m) =>
    SKPicture ->
    -- | \"nested\"; If true, include the op-counts of nested pictures as well,
    -- else just return count the ops in the top-level picture.
    Bool ->
    m Int
approximateOpCount picture nested = evalContIO do
    picture' <- useObj picture
    liftIO $ fmap fromIntegral $ sk_picture_approximate_op_count picture' (fromBool nested)

{- | Returns the approximate byte size of SkPicture. Does not include large
objects referenced by 'SKPicture'.
-}
approximateBytesUsed ::
    (MonadIO m) =>
    SKPicture ->
    m Int
approximateBytesUsed picture = evalContIO do
    picture' <- useObj picture
    liftIO $ fmap fromIntegral $ sk_picture_approximate_bytes_used picture'
