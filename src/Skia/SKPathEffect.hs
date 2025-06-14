module Skia.SKPathEffect where

import Linear
import Skia.Internal.Prelude

-- TODO: Figure out why SkiaSharp's binding/SkiaSharp/SKPathEffect.cs does not
-- use @sk_path_effect_unref@.

createCompose ::
    (MonadIO m) =>
    -- | Outer
    SKPathEffect ->
    -- | Inner
    SKPathEffect ->
    m SKPathEffect
createCompose outer inner = evalContIO do
    outer' <- useObj outer
    inner' <- useObj inner
    result' <- liftIO $ sk_path_effect_create_compose outer' inner'
    toObjectFin sk_path_effect_unref result'

createSum ::
    (MonadIO m) =>
    -- | First
    SKPathEffect ->
    -- | Second
    SKPathEffect ->
    m SKPathEffect
createSum first second = evalContIO do
    first' <- useObj first
    second' <- useObj second
    result' <- liftIO $ sk_path_effect_create_sum first' second'
    toObjectFin sk_path_effect_unref result'

createDiscrete ::
    (MonadIO m) =>
    -- | Segment length
    Float ->
    -- | Deviation
    Float ->
    -- | Seed assist
    Word32 ->
    m SKPathEffect
createDiscrete segLength deviation seedAssist = liftIO do
    result' <- sk_path_effect_create_discrete (coerce segLength) (coerce deviation) seedAssist
    toObjectFin sk_path_effect_unref result'

createCorner ::
    (MonadIO m) =>
    -- | Radius
    Float ->
    m SKPathEffect
createCorner radius = liftIO do
    result' <- sk_path_effect_create_corner (coerce radius)
    toObjectFin sk_path_effect_unref result'

create1DPath ::
    (MonadIO m) =>
    SKPath ->
    -- | Advance
    Float ->
    -- | Phase
    Float ->
    SKPathEffect1DStyle ->
    m SKPathEffect
create1DPath path advance phase style = evalContIO do
    path' <- useObj path
    result' <- liftIO $ sk_path_effect_create_1d_path path' (coerce advance) (coerce phase) (marshalSKEnum style)
    toObjectFin sk_path_effect_unref result'

create2DLine ::
    (MonadIO m) =>
    -- | Width
    Float ->
    -- | Matrix
    M33 Float ->
    m SKPathEffect
create2DLine width matrix = evalContIO do
    matrix' <- useStorable $ toSKMatrix matrix
    result' <- liftIO $ sk_path_effect_create_2d_line (coerce width) matrix'
    toObjectFin sk_path_effect_unref result'

create2DPath ::
    (MonadIO m) =>
    M33 Float ->
    SKPath ->
    m SKPathEffect
create2DPath matrix path = evalContIO do
    matrix' <- useStorable $ toSKMatrix matrix
    path' <- useObj path
    result' <- liftIO $ sk_path_effect_create_2d_path matrix' path'
    toObjectFin sk_path_effect_unref result'

createDashRaw ::
    (MonadIO m) =>
    -- | Intervals array
    Ptr Float ->
    -- | Intervals array length
    Int ->
    -- | Phase
    Float ->
    m SKPathEffect
createDashRaw intervals intervalsLen phase = liftIO do
    result' <- sk_path_effect_create_dash (castPtr intervals) (fromIntegral intervalsLen) (coerce phase)
    toObjectFin sk_path_effect_unref result'

createDashByList ::
    (MonadIO m) =>
    -- | Intervals
    [Float] ->
    -- | Phase
    Float ->
    m SKPathEffect
createDashByList intervals phase = liftIO do
    withArrayLen intervals \intervalsLen intervals' -> do
        createDashRaw intervals' intervalsLen phase

createTrim ::
    (MonadIO m) =>
    -- | Start
    Float ->
    -- | Stop
    Float ->
    SKPathEffectTrimMode ->
    m SKPathEffect
createTrim start stop mode = liftIO do
    result' <- sk_path_effect_create_trim (coerce start) (coerce stop) (marshalSKEnum mode)
    toObjectFin sk_path_effect_unref result'
